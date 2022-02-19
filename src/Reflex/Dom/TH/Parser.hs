-- | 

module Reflex.Dom.TH.Parser
( TElement(..),
  AttributeType(..),
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
import Language.Haskell.TH.Syntax

type Parser = Parsec Void String
type TTag = String
data AttributeType = Static | Dynamic deriving (Show, Lift)
type Attribute = (AttributeType, String, String)
type Ref = Int
data TElement = TElement { tTag :: TTag
                         , tRef :: Maybe Ref
                         , tAttrs :: [Attribute]
                         , tChilds :: [TElement] }
               | TText String
               | TComment String
               | TWidget String
               deriving Show


openTag :: Parser (String, Maybe Int, [Attribute])
openTag =  
     between (char '<') (char '>') $ do
       tag <- many (alphaNumChar <|> char '-')
       space
       ref <-  optional $ do
         void $ char '#'
         L.decimal <* space
       attrs <- attributes
       space
       return (tag, ref, attrs)

closeTag :: String -> Parser ()
closeTag tag = void $ between (string "</") (space >> char '>') (string tag)

comment :: Parser TElement
comment = TComment <$> ((string "<!--") *> (manyTill anySingle (string "-->")))


stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

attribute :: Parser Attribute
attribute = (Static,,) <$>  (some alphaNumChar <* char '=') <*> stringLiteral


attributes :: Parser [Attribute]
attributes = sepBy attribute space1 <* space
                     

node :: Parser TElement
node = do
  (tag, ref, attrs) <- openTag
  space
  childs <- manyTill element (closeTag tag)
  return $ TElement tag ref attrs childs


widget :: Parser TElement
widget =  TWidget <$> (string "{{" *> manyTill anySingle (string "}}"))

text :: Parser TElement
text =  TText <$>  dropWhileEnd isSpace <$>  someTill anySingle (lookAhead (char '<' *> return () <|> string "{{" *> return () ))

element :: Parser TElement     
element = (comment <|>  node <|> widget <|> text) <* space
  
template :: Parser [TElement]
template = do
  space
  result <- many element
  eof
  return result


parseTemplate :: FilePath -> String -> Either (ParseErrorBundle String Void) [TElement]
parseTemplate fn = runParser template fn
