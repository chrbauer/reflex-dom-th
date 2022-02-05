-- | 

module Reflex.Dom.TH where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Text (Text)

type Parser = Parsec Void String
type Tag = String
data Element = Element Tag [Element]
               | Text String
               deriving Show

openTag :: Parser String 
openTag =  try $ between (char '<') (space >> char '>') (many alphaNumChar)

closeTag :: String -> Parser String 
closeTag tag =  between (string "</") (space >> char '>') (string tag) 


element :: Parser Element
element = do
  tag <- openTag
  childs <- many element
  closeTag tag
  return $ Element tag childs

