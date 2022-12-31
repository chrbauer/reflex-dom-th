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
import Data.List (insert, sortBy)
import Data.Array
import Data.Text (Text)
import qualified Data.Text as T
import Data.Function (on)
import Instances.TH.Lift()
import Control.Monad.Reader

import I18N.Gettext.TH (gettext)

type Ref = Int

data ChildResult =
   CREmpty
 | CRSimple Ref
 | CRTuple (Maybe Ref) [Ref]
 deriving Show

type VarEnv a = Reader (Ref -> Name) a

data Chain = CBind CElement ChildResult Chain | CResult [Ref]
  deriving Show

data CElement = CElement { cTag :: String
                         , cSiblingsRefs :: [Ref]
                         , cChildRefs :: [Ref]
                         , cOutRefs :: [Ref]
                         , cMyRef :: Maybe Ref
                         , cAttrs :: [(Text, Text)]
                         , cDynAttrs :: Maybe String
                         , cChilds :: Chain }
               | CText String
               | CGettext String
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
    elem' = CElement tTag inRefs childRefs outRefs tRef attrs tDynAttrs childChain
    childChain = compile tChilds []
    childRefs = chainOut childChain
    outRefs = maybe id insert tRef childRefs 
    expRefs = merge inRefs outRefs
    attrs    = sortBy (compare `on` fst) [ (T.pack k, T.pack v) | (k, v) <- tAttrs ]


compile (TWidget w r:etail)  inRefs =   CBind (CWidget w r) (maybe CREmpty CRSimple r) (compile etail expRefs)
    where expRefs = maybe id insert r inRefs
compile (e:etail) inRefs =
      CBind (toC e) CREmpty (compile etail inRefs)
  where
    toC (TText t) = CText t
    toC (TGettext t) = CGettext t
    toC (TComment c) = CComment c
    toC _ = error "internal"
                           
withVarF :: MonadReader t m => (t -> b) -> m b
withVarF f = ask >>= \ var -> return (f var)

opt :: (Ref -> Name) -> Maybe Ref -> Q Pat
opt var = maybe wildP $ varP . var

tupArg :: (t -> Name) -> [t] -> PatQ
tupArg var [x]   = varP $ var x
tupArg var args  = tupP $ map (varP . var) args

clambda :: ChildResult -> ExpQ -> VarEnv ExpQ
clambda CREmpty   e    =  return $ lamE [wildP] e
clambda (CRSimple v)  e =   withVarF $ \ var -> lamE [varP $ var v] e
clambda (CRTuple Nothing crefs) e =  withVarF  $ \ var -> lamE [tupArg var crefs] e
clambda (CRTuple mref crefs) e =  withVarF  $ \ var -> lamE [tupP [ opt var mref
                                                                  , tupArg var crefs]] e

                                     
elWithAttr :: String -> [(Text, Text)] -> Maybe String -> ExpQ
elWithAttr tag [] Nothing = [| el tag |]
elWithAttr tag [] (Just dynAttr) = [| elDynAttr tag $(unboundVarE $ mkName dynAttr) |]
elWithAttr tag [("class", cl)] Nothing = [| elClass tag cl |]
elWithAttr tag attr Nothing = [| elAttr tag (M.fromAscList attr) |]
elWithAttr tag attr (Just dynAttr) = [| elDynAttr tag (flip M.union (M.fromAscList attr) <$>  $(unboundVarE $ mkName dynAttr)) |]

el'WithAttr :: String -> [(Text, Text)] -> Maybe String  -> ExpQ
el'WithAttr tag [] Nothing = [| el' tag |]
el'WithAttr tag [] (Just dynAttr) = [| elDynAttr' tag $(unboundVarE $ mkName dynAttr) |]
el'WithAttr tag [("class", cl)] Nothing = [| elClass' tag cl |]
el'WithAttr tag attr Nothing = [| elAttr' tag (M.fromAscList attr) |]
el'WithAttr tag attr (Just dynAttr) = [| elDynAttr' tag (flip M.union (M.fromAscList attr) <$>  $(unboundVarE $ mkName dynAttr)) |]

tupRes :: (t -> Name) -> [t] -> ExpQ
tupRes var [a] = varE $ var a
tupRes var l   = tupE $ map (varE . var) l

cchain :: Chain ->  VarEnv ExpQ
cchain (CResult orefs)  = do
  var <- ask
  return (appE (varE 'return) (tupRes var orefs))
cchain (CBind ce cres rest)  = do
  n <- cnode ce
  r <- cchain rest
  l <- clambda cres r
  return [| $(n) >>=  $(l) |]


cnode :: CElement -> VarEnv ExpQ
cnode (CElement tag _ _ _ Nothing attr tDynAttrs childs) = cchain childs >>= \ cs ->
     return [|  $(elWithAttr tag attr tDynAttrs) $(cs) |]
cnode  (CElement tag _ _ _ (Just _) attr tDynAttrs childs) = cchain childs >>= \ cs ->
     return [| $(el'WithAttr tag attr tDynAttrs) $(cs) |]
cnode (CText "") = return $ [| blank |]
cnode (CText txt) = return $ [| text txt |]
cnode (CGettext txt') =   
  return $ appE [| text |] (quoteExp gettext txt')
cnode (CWidget x _) = return $ unboundVarE $ mkName x
cnode (CComment txt) = return [| comment txt |]

chainOut :: Chain -> [Ref] 
chainOut (CBind _ _ next) = chainOut next
chainOut (CResult out) = out

domExp :: [TElement] -> Q Exp
domExp result =
  let refchain = compile result []
      out = chainOut refchain
  in do
    varNames <-  listArray (0, length out) <$> mapM (\ r -> newName ("r" ++ show r)) out
    runReader (cchain refchain) (varNames !)

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
  
