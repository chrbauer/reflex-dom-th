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
--import Language.Haskell.TH.Syntax

type Parser = Parsec Void String
type TTag = String
type Attribute = (String, String)
type Ref = Int
data TElement = TElement { tTag :: TTag
                         , tRef :: Maybe Ref
                         , tAttrs :: [Attribute]
                         , tDynAttrs :: Maybe String
                         , tChilds :: [TElement] }
               | TText String
               | TGettext String
               | TComment String
               | TWidget String (Maybe Ref)
               deriving Show

refOpt :: Parser (Maybe Int)
refOpt = optional . try $ do
      space1
      void $ char '#'
      L.decimal <* space

openTag :: Parser (String, [TElement] -> TElement)
openTag =  
     between (char '<') (char '>') $ do
       tag <- many (alphaNumChar <|> char '-')
       ref <- refOpt
       space
       attrs <- attributes
       space
       dynAttr <- optional varRef
       return $ (tag, TElement tag ref attrs dynAttr)

closeTag :: String -> Parser ()
closeTag tag = void $ between (string "</" >> space) (char '>') (string tag >> space)

comment :: Parser TElement
comment = TComment <$> ((string "<!--") *> (manyTill anySingle (string "-->")))


stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

attribute :: Parser Attribute
attribute = (,) <$>  (some (alphaNumChar <|> char '-') <* char '=') <*> stringLiteral


attributes :: Parser [Attribute]
attributes = sepEndBy attribute space1 <* space
                     

node :: Parser TElement
node = do
  (tag, mkElem) <- openTag
  space
  childs <- manyTill element (closeTag tag)
  return $ mkElem childs

varName :: Parser String
varName = (:) <$> lowerChar <*> many alphaNumChar

varRef :: Parser String
varRef =  string "{{" *> space *> varName <* string "}}" <* space

gettext :: Parser TElement
gettext = TGettext <$> (string "[__|" *> space *> manyTill anySingle (string "|]"))


widget :: Parser TElement
widget =  TWidget <$> (string "{{" *> space *> varName) <*> (refOpt <* (string "}}"))

text :: Parser TElement
text =  TText <$>  dropWhileEnd isSpace <$>  someTill anySingle (lookAhead (char '<' *> return () <|> string "{{" *> return () ))

element :: Parser TElement     
element = (comment <|>  node <|> widget <|> gettext <|> text) <* space
  
template :: Parser [TElement]
template = do
  space
  result <- many element
  eof
  return result


parseTemplate :: FilePath -> String -> Either (ParseErrorBundle String Void) [TElement]
parseTemplate fn = runParser template fn
