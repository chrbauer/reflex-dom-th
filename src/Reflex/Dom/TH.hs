-- | 

module Reflex.Dom.TH where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Text (Text)

type Parser = Parsec Void String

tag :: Parser String 
tag =  between (char '<') (char '>') (many alphaNumChar) 


