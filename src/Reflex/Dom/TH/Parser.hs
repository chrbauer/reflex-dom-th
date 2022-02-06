-- | 

module Reflex.Dom.TH.Parser
( TElement(..),
  parseTemplate
  )
where

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
  space
  try $ between (char '<') (space >> char '>') (many alphaNumChar)

closeTag :: String -> Parser String 
closeTag tag =  do
  space
  between (string "</") (space >> char '>') (string tag) 


element :: Parser TElement
element = do
  tag <- openTag
  childs <- many element
  closeTag tag
  return $ TElement tag childs


template = do
  result <- element
  space
  eof
  return result

parseTemplate fn = runParser template fn
