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
import Data.Void
import Data.Text (Text)

type Parser = Parsec Void String
type TTag = String
type Attributes = [(String, String)]
data TElement = TElement TTag Attributes [TElement]
               | TText String
               deriving Show

openTag :: Parser (String, Attributes)
openTag =  
     between (char '<') (space >> char '>') $ do
       tag <- many (alphaNumChar <|> char '-')
       attrs <- attributes
       return (tag, attrs)

closeTag :: String -> Parser String 
closeTag tag =  do
  between (string "</") (space >> char '>') (string tag) 


quote = char '"'

attribute :: Parser (String, String)
attribute = (,) <$>  (some alphaNumChar <* char '=') <*>
                     between quote quote (many (satisfy (/= '"')))

attributes :: Parser Attributes
attributes = many (space1 *> attribute)
                     

node :: Parser TElement
node = do
  (tag, attrs) <- openTag
  childs <- many element
  space
  closeTag tag
  return $ TElement tag attrs childs

text = do
     t <- dropWhileEnd isSpace <$> some (satisfy (/= '<'))
     return $ TText  t

element :: Parser TElement     
element = try $ do
  space
  node <|> text
  
template :: Parser TElement
template = do
  result <- element
  space
  eof
  return result


parseTemplate :: FilePath -> String -> Either (ParseErrorBundle String Void) TElement
parseTemplate fn = runParser template fn
