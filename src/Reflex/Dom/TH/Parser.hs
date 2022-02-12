-- | 

module Reflex.Dom.TH.Parser
( TElement(..),
  parseTemplate
  )
where

import Data.Char
import Data.List


import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L 
import Data.Void
import Control.Monad

type Parser = Parsec Void String
type TTag = String
type Attributes = [(String, String)]
data TElement = TElement TTag Attributes [TElement]
               | TText String
               | TComment String
               deriving Show


openTag :: Parser (String, Attributes)
openTag =  
     between (char '<') (space >> char '>') $ do
       tag <- many (alphaNumChar <|> char '-')
       space
       attrs <- attributes
       return (tag, attrs)

closeTag :: String -> Parser ()
closeTag tag = void $ between (string "</") (space >> char '>') (string tag)

comment :: Parser TElement
comment = TComment <$> ((string "<!--") *> (manyTill anySingle (string "-->")))


stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

attribute :: Parser (String, String)
attribute = (,) <$>  (some alphaNumChar <* char '=') <*> stringLiteral


attributes :: Parser Attributes
attributes = sepBy attribute space1 <* space
                     

node :: Parser TElement
node = do
  (tag, attrs) <- openTag
  space
  childs <- manyTill element (closeTag tag)
  return $ TElement tag attrs childs

text :: Parser TElement
text =  TText <$>  dropWhileEnd isSpace <$> some (satisfy (/= '<'))

element :: Parser TElement     
element = (comment <|>  node <|> text) <* space
  
template :: Parser [TElement]
template = do
  space
  result <- many element
  eof
  return result


parseTemplate :: FilePath -> String -> Either (ParseErrorBundle String Void) [TElement]
parseTemplate fn = runParser template fn
