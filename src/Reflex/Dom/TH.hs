-- | 

module Reflex.Dom.TH where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Text (Text)
import Language.Haskell.TH.Quote

import Reflex.Dom.TH.Parser
import Reflex.Dom 

instantiate :: TElement -> ExpQ
instantiate  (TElement name []) = [| el name $ blank |]

dom :: QuasiQuoter
dom = QuasiQuoter
  { quoteExp  = \str ->
      case parseTemplate "" str of
        Left err -> fail $ show err
        Right x  ->  instantiate x
  , quotePat  = error "Usage as a parttern is not supported"
  , quoteType = error "Usage as a type is not supported"
  , quoteDec = error "Usage as a decl is not supported"
  }
