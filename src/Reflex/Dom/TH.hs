-- | 

module Reflex.Dom.TH where


import Language.Haskell.TH.Quote
import Language.Haskell.TH

import Reflex.Dom.TH.Parser
import Reflex.Dom.Widget.Basic
import qualified Data.Map as M

instantiate :: TElement -> ExpQ
instantiate  (TElement name [] cs) = [| el name $(childs cs)  |]
instantiate  (TElement name attr cs) = [| elAttr name (M.fromList attr) $(childs cs)  |]
instantiate  (TText "") = [| blank |]
instantiate  (TText txt) = [| text txt |]

childs :: [TElement] -> ExpQ
childs [] = [| blank |]
childs [x] = instantiate x
childs (h:t) = [|  $(instantiate h) >> $(childs t) |]

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
