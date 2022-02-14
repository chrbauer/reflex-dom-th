-- | 

module Reflex.Dom.TH
  (dom, domFile)
where


import Text.Megaparsec.Error

import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Reflex.Dom.TH.Parser
import Reflex.Dom.Widget.Basic
import qualified Data.Map as M



node :: TElement -> ExpQ
node  (TElement name Nothing [] cs) = [| el name $(nodes cs)  |]
node  (TElement name Nothing attr cs) = [| elAttr name (M.fromList [ (k, v) | (Static, k, v) <- attr ]) $(nodes cs)  |]
node  (TText "") = [| blank |]
node  (TText txt) = [| text txt |]
node  (TWidget x) = unboundVarE $ mkName x
node  (TComment txt) = [| comment txt |]


nodes :: [TElement] -> ExpQ
nodes [] = [| blank |]
nodes [x] = node x
nodes (h:t) = [|  $(node h) >> $(nodes t) |]

dom :: QuasiQuoter
dom = QuasiQuoter
  { quoteExp  = \str ->
      case parseTemplate "" str of
        Left err -> fail $ errorBundlePretty err
        Right result ->  nodes result
  , quotePat  = error "Usage as a parttern is not supported"
  , quoteType = error "Usage as a type is not supported"
  , quoteDec = error "Usage as a decl is not supported"

  }


domFile :: FilePath -> Q Exp
domFile path = do
  str <- runIO (readFile path)
  addDependentFile path
  case parseTemplate path str of
        Left err -> fail $ errorBundlePretty err
        Right result  ->  nodes result
  
