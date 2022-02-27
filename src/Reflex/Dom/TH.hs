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
import Data.List (insert)
import Data.Array

type Ref = Int

data ChildResult =
   CREmpty
 | CRSimple Ref
 | CRTuple (Maybe Ref) [Ref]
 deriving Show

data Chain = CBind CElement ChildResult Chain | CResult [Ref]
  deriving Show

data CElement = CElement { cTag :: String
                         , cSiblingsRefs :: [Ref]
                         , cChildRefs :: [Ref]
                         , cOutRefs :: [Ref]
                         , cMyRef :: Maybe Ref
                         , cAttrs :: [(String, String)]
                         , cDynAttrs :: Maybe String
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


compile :: [TElement] -> [Ref] -> Chain
compile [] inRefs = CResult inRefs
compile ((TElement {..}):etail) inRefs =
      CBind elem' (CRTuple tRef childRefs) (compile etail expRefs)
  where
    elem' = CElement tTag inRefs childRefs outRefs tRef tAttrs childChain
    childChain = compile tChilds []
    childRefs = chainOut childChain
    outRefs = maybe id insert tRef childRefs 
    expRefs = merge inRefs outRefs


compile (TWidget w r:etail)  inRefs =   CBind (CWidget w r) (maybe CREmpty CRSimple r) (compile etail expRefs)
    where expRefs = maybe id insert r inRefs
compile (e:etail) inRefs =
      CBind (toC e) CREmpty (compile etail inRefs)
  where
    toC (TText t) = CText t
    toC (TComment c) = CComment c
    toC _ = error "internal"
                           


opt :: (Ref -> Name) -> Maybe Ref -> Q Pat
opt var = maybe (runQ [p| () |]) $ varP . var

clambda :: (Ref -> Name) -> ChildResult -> ExpQ -> ExpQ
clambda _    CREmpty       =  lamE [wildP]
clambda var (CRSimple v)  =  lamE [tupP [varP $ var v]]
clambda var (CRTuple Nothing crefs)  =  lamE [tupP $ map (varP . var) crefs]
clambda var (CRTuple mref crefs)  =  lamE [tupP [ opt var mref
                                                , tupP $ map (varP . var) crefs]]

                                     
elWithAttr :: String -> [(String, String)] -> Maybe String -> ExpQ
elWithAttr tag [] Nothing = [| el tag |]
elWithAttr tag [] (Just dynAttr) = [| elDynAttr tag $(unboundVarE $ mkName dynAttr) |]
elWithAttr tag [("class", cl)] Nothing = [| elClass tag cl |]
elWithAttr tag attr Nothing = [| elAttr tag (M.fromList attr) |]
elWithAttr tag attr (Just dynAttr) = [| elDynAttr tag (flip M.union (M.fromList attr) <$> dnyAttr) |]

el'WithAttr :: String -> [(String, String)] -> ExpQ
el'WithAttr tag [] Nothing = [| el' tag |]
el'WithAttr tag [] (Just dynAttr) = [| elDynAttr' tag $(unboundVarE $ mkName dynAttr) |]
el'WithAttr tag [("class", cl)] Nothing = [| elClass' tag cl |]
el'WithAttr tag attr Nothing = [| elAttr' tag (M.fromList attr) |]
el'WithAttr tag attr (Just dynAttr) = [| elDynAttr' tag (flip M.union (M.fromList attr) <$> dnyAttr) |]


cchain :: (Ref -> Name) -> Chain ->  ExpQ
cchain var (CResult orefs)  = (appE (varE 'return) (tupE $ map (varE . var) orefs))
cchain var (CBind ce cres rest)  = [| $(cnode var ce) >>=  $(clambda var cres (cchain var rest)) |]

cnode :: (Ref -> Name) -> CElement -> ExpQ
cnode var (CElement tag _ _ _ Nothing attr childs) = [|  $(elWithAttr tag attr) $(cchain var childs)|]
cnode var (CElement tag _ _ _ (Just _) attr childs) = [| $(el'WithAttr tag attr) $(cchain var childs) |]
cnode _ (CText "") = [| blank |]
cnode _ (CText txt) = [| text txt |]
cnode _ (CWidget x _) = unboundVarE $ mkName x
cnode _ (CComment txt) = [| comment txt |]

chainOut :: Chain -> [Ref] 
chainOut (CBind _ _ next) = chainOut next
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
  
