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
import Data.Map (Map)
--import Data.Maybe
import Data.List (insert)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Array

type Ref = Int
data Chain = CBind CElement (Maybe Ref) [Ref] Chain | CResult [Ref]
  deriving Show

data CElement = CElement { cTag :: String
                         , cSiblingsRefs :: [Ref]
                         , cChildRefs :: [Ref]
                         , cOutRefs :: [Ref]
                         , cMyRef :: Maybe Ref
                         , cAttrs :: [(String, String)]
                         , cChilds :: Chain }
               | CText String
               | CComment String
               | CWidget String (Maybe Ref)
               deriving Show

merge :: Ord a => [a] -> [a] -> [a]
merge a [] = a
merge [] b = b
merge  a@(ah:at) b@(bh:bt)
  | compare ah bh == GT = bh : merge a bt
  | otherwise = ah : merge at b


--  do (r1, (r1a, r1b)) <- el1 $ el1a >>= \ r1a -> el1b >>= \r1b -> return (r1a, r1b)



compile :: [TElement] -> [Ref] -> Chain
compile [] inRefs = CResult inRefs
compile ((TElement {..}):etail) inRefs =
      CBind elem' tRef childRefs (compile etail expRefs)
  where
    elem' = CElement tTag inRefs childRefs outRefs tRef attrs childChain
    childChain = compile tChilds []
    childRefs = chainOut childChain
    outRefs = maybe id insert tRef childRefs 
    expRefs = merge inRefs outRefs
    attrs = [ (k, v) | (Static, k, v) <- tAttrs ]

compile (TWidget w r:etail)  inRefs =   CBind (CWidget w r) r expRefs (compile etail expRefs)
    where expRefs = maybe id insert r inRefs
compile (e:etail) inRefs =
      CBind (toC e) Nothing inRefs (compile etail inRefs)
  where
    toC (TText text) = CText text
    toC (TComment comment) = CComment comment
    toC _ = error "internal"
                           


opt :: (Ref -> Name) -> Maybe Ref -> Q Pat
opt var = maybe (runQ [p| () |]) $ varP . var

clambda var Nothing crefs   =  lamE [tupP $ map (varP . var) crefs ]
clambda var mref crefs =  lamE [tupP [ opt var mref
                           , tupP $ map (varP . var) crefs]]



elWithAttr :: String -> [(String, String)] -> ExpQ
elWithAttr tag [] = [| el tag |]
elWithAttr tag [("class", cl)] = [| elClass tag cl |]
elWithAttr tag attr = [| elAttr tag (M.fromList attr) |]

el'WithAttr :: String -> [(String, String)] -> ExpQ
el'WithAttr tag [] = [| el' tag |]
el'WithAttr tag [("class", cl)] = [| elClass' tag cl |]
el'WithAttr tag attr = [| elAttr' tag (M.fromList attr) |]


cchain :: (Ref -> Name) -> Chain ->  ExpQ
cchain var (CResult orefs)  = (appE (varE 'return) (tupE $ map (varE . var) orefs))
cchain var (CBind ce mref crefs rest)  = [| $(cnode var ce) >>=  $(clambda var mref crefs (cchain var rest)) |]

{-
cnodes :: (Ref -> Name) -> Chain ->  ExpQ

cnodes var [e@(CElement _ _ crefs orefs mref _ _)]  
    | null orefs = [| $(cnode var e) |]
    | otherwise = [| $(cnode var e) >>=  $(clambda var mref crefs
                                             (appE (varE 'return) (tupE $ map (varE . var) orefs))) |]
cnodes var (e@(CElement _ _ crefs _ mref _ _):rest)  = [| $(cnode var e) >>=  $(clambda var mref crefs (cnodes var rest)) |]
cnodes  var [e] = cnode var e
cnodes var (h:t)  = [|  $(cnode var h) >> $(cnodes var t) |]
-}

cnode :: (Ref -> Name) -> CElement -> ExpQ
cnode var (CElement tag _ _ _ Nothing attr childs) = [|  $(elWithAttr tag attr) $(cchain var childs)|]
cnode var (CElement tag _ _ _ (Just _) attr childs) = [| $(el'WithAttr tag attr) $(cchain var childs) |]
cnode _ (CText "") = [| blank |]
cnode _ (CText txt) = [| text txt |]
cnode _ (CWidget x _) = unboundVarE $ mkName x
cnode _ (CComment txt) = [| comment txt |]

chainOut :: Chain -> [Ref] 
chainOut (CBind _ _ _ next) = chainOut next
chainOut (CResult out) = out

domExp :: [TElement] -> Q Exp
domExp result =
  let refchain = compile result []
      out = chainOut refchain
  in do
    varNames <-  listArray (0, length out) <$> mapM (\ r -> newName ("r" ++ show r)) out
    cchain (varNames !) refchain

dom :: QuasiQuoter
dom = QuasiQuoter
  { quoteExp  = \str ->
      case parseTemplate "" str of
        Left err -> fail $ errorBundlePretty err
        Right result -> domExp result
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
        Right result  ->  domExp result
  
