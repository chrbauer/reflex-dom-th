-- | 

module Reflex.Dom.TH
  (dom, domFile, merge)
where


import Text.Megaparsec.Error

import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Reflex.Dom.TH.Parser
import Reflex.Dom.Widget.Basic 
import qualified Data.Map as M
import Data.Map (Map)
--import Data.Maybe
import Data.List (insert)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Array

type Ref = Int
data CElement = CElement { cTag :: String
                         , cSiblingsRefs :: [Ref]
                         , cChildRefs :: [Ref]
                         , cOutRefs :: [Ref]
                         , cMyRef :: Maybe Ref
                         , cAttrs :: [(String, String)]
                         , cChilds :: [CElement] }
               | CText String
               | CComment String
               | CWidget String
               deriving Show

merge :: Ord a => [a] -> [a] -> [a]
merge a [] = a
merge [] b = b
merge  a@(ah:at) b@(bh:bt)
  | compare ah bh == GT = bh : merge a bt
  | otherwise = ah : merge at b


--  do (r1, (r1a, r1b)) <- el1 $ el1a >>= \ r1a -> el1b >>= \r1b -> return (r1a, r1b)



compile :: [TElement] -> [CElement] -> [Ref] -> ([CElement], [Ref])
compile [] acc inRefs = (reverse acc, inRefs)
compile ((TElement {..}):etail) acc inRefs =
      compile etail (elem':acc) expRefs
  where
    elem' = CElement tTag inRefs childRefs outRefs tRef attrs childs
    (childs, childRefs) = compile tChilds [] []
    outRefs = maybe id insert tRef childRefs
    expRefs = merge inRefs outRefs
    attrs = [ (k, v) | (Static, k, v) <- tAttrs ]
compile (elem:etail) acc inRefs =
      compile etail (toC elem : acc) inRefs
  where
    toC (TText text) = CText text
    toC (TComment comment) = CComment comment
    toC (TWidget widget) = CWidget widget
    toC _ = undefined
                           


node :: TElement -> ExpQ
node  (TElement name Nothing [] cs) = [| el name $ do $(nodes cs)  |]
node  (TElement name (Just i) [] cs) = [| fst <$> el' name $(nodes cs)  |]
node  (TElement name Nothing attr cs) = [| elAttr name (M.fromList [ (k, v) | (Static, k, v) <- attr ]) $(nodes cs)  |]
node  (TText "") = [| blank |]
node  (TText txt) = [| text txt |]
node  (TWidget x) = unboundVarE $ mkName x
node  (TComment txt) = [| comment txt |]


nodes :: [TElement] -> ExpQ
nodes [] = [| blank |]
nodes [x] = node x
nodes (h:t) = [|  $(node h) >> $(nodes t) |]

opt :: (Ref -> Name) -> Maybe Ref -> Q Pat
opt var = maybe (runQ [p| () |]) $ varP . var

clambda var Nothing crefs   =  lamE [tupP $ map (varP . var) crefs ]
clambda var mref crefs =  lamE [tupP [ opt var mref
                           , tupP $ map (varP . var) crefs]]


cnodes :: (Ref -> Name) -> [CElement] ->  ExpQ
cnodes _ []  = [| blank |]
cnodes var [elem@(CElement _ _ crefs orefs mref _ _)]  = [| $(cnode var elem) >>=  $(clambda var mref crefs
                                                                                        (appE (varE 'return) (tupE $ map (varE . var) orefs))) |]
cnodes var (elem@(CElement _ _ crefs orefs mref _ _):rest)  = [| $(cnode var elem) >>=  $(clambda var mref crefs (cnodes var rest)) |]
                                                         
cnodes  var [elem] = cnode var elem
cnodes var (h:t)  = [|  $(cnode var h) >> $(cnodes var t) |]

cnode :: (Ref -> Name) -> CElement -> ExpQ
cnode var (CElement tag _ _ _ Nothing attr childs) = [|  elAttr tag attr $(cnodes var childs)|]
cnode var (CElement tag _ _ _ (Just _) attr childs) = [|  el' tag attr $(cnodes var childs) |]
cnode _ (CText "") = [| blank |]
cnode _ (CText txt) = [| text txt |]
cnode _ (CWidget x) = unboundVarE $ mkName x
cnode _ (CComment txt) = [| comment txt |]

dom :: QuasiQuoter
dom = QuasiQuoter
  { quoteExp  = \str ->
      case parseTemplate "" str of
        Left err -> fail $ errorBundlePretty err
        Right result -> 
          let (cns, out) = compile result [] [] in do
            varNames <-  listArray (0, length out) <$> mapM (\ r -> newName ("r" ++ show r)) out
            cnodes (varNames !) cns
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
  
