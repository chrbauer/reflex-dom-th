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
data TElement = TElement TTag [TElement]
               | TText String
               deriving Show

openTag :: Parser String 
openTag =  do
  try $ between (char '<') (space >> char '>') (many alphaNumChar)

closeTag :: String -> Parser String 
closeTag tag =  do
  between (string "</") (space >> char '>') (string tag) 


node :: Parser TElement
node = do
  tag <- openTag
  childs <- many element
  closeTag tag
  return $ TElement tag childs

text = do
     t <- dropWhileEnd isSpace <$> some (satisfy (/= '<'))
     return $ TText  t

element :: Parser TElement     
element = do
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
