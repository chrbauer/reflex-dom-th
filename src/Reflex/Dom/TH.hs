-- | 

module Reflex.Dom.TH
  (dom, domFile)
where


import Text.Megaparsec.Error
import System.Directory 

import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Reflex.Dom.TH.Parser
import Reflex.Dom.Widget.Basic
import qualified Data.Map as M




instantiate :: TElement -> ExpQ
instantiate  (TElement name [] cs) = [| el name $(childs cs)  |]
instantiate  (TElement name attr cs) = [| elAttr name (M.fromList attr) $(childs cs)  |]
instantiate  (TText "") = [| blank |]
instantiate  (TText txt) = [| text txt |]
instantiate  (TComment txt) = [| comment txt |]

childs :: [TElement] -> ExpQ
childs [] = [| blank |]
childs [x] = instantiate x
childs (h:t) = [|  $(instantiate h) >> $(childs t) |]

dom :: QuasiQuoter
dom = QuasiQuoter
  { quoteExp  = \str ->
      case parseTemplate "" str of
        Left err -> fail $ errorBundlePretty err
        Right node ->  instantiate node
  , quotePat  = error "Usage as a parttern is not supported"
  , quoteType = error "Usage as a type is not supported"
  , quoteDec = error "Usage as a decl is not supported"

  }


domFile :: FilePath -> Q Exp
domFile path = do
  runIO $ do
     cwd <- getCurrentDirectory
     putStrLn cwd
  str <- runIO (readFile path)
  addDependentFile path
  case parseTemplate "" str of
        Left err -> fail $ errorBundlePretty err
        Right node  ->  instantiate node
  
