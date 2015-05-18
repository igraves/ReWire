{-# LANGUAGE OverloadedStrings #-}
module ReWire.RouteLogic.CodeGen where
import ReWire.RouteLogic.Types

import Data.Functor.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Except
import Control.Monad
import Control.Monad.Trans
import Data.List (find,intercalate,foldl')
import qualified Data.Map as M

import Language.Haskell.Exts.Build
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.SrcLoc (noLoc)
import Language.Haskell.Exts.Pretty

import qualified Data.Text as T

import Debug.Trace

---Computation Types and Functions---
data Exception = E String deriving Show
type Builder = ReaderT (RouteTable,ArgMap,RefMap) (ExceptT Exception Identity)
type ArgMap = [(NodeRef,AnchorName,Int)]
type RefMap = [(NodeRef,(NodeId -> Node))]


buildRT :: LinkNodes -> Builder RouteTable
buildRT (LNS ls) = foldM ff (RouteTable [] []) ls 
  where
    ff (RouteTable nodes links) (L l) = return $ RouteTable nodes (l:links) 
    ff rt                       (N _ "input")  = return $ rt
    ff rt                       (N _ "output") = return $ rt
    ff (RouteTable nodes links) (N nid nref) = do
                                                  (_,_,rm) <- ask
                                                  nf <- liftMaybe "NodeRef undefined" $ lookup nref rm
                                                  return $ RouteTable ((nf nid):nodes) links
    

args :: ArgMap -> NodeRef -> Int
args m nr = length $ filter (\(x,_,_) -> x == nr) m

argIndex :: ArgMap -> NodeRef -> AnchorName -> Maybe Int
argIndex m nr an = liftM (\(_,_,i) -> i) $ find (\(x,y,_) -> (x == nr) && (y == an)) m

getAnchors :: ArgMap -> NodeRef -> [AnchorName]
getAnchors m nr = map (\(_,a,_) -> a) $ filter (\(x,_,_) -> x == nr) m  

liftMaybe :: String -> Maybe a -> Builder a
liftMaybe e Nothing  = lift $ throwError (E e)
liftMaybe _ (Just x) = return x

runBuilder :: (RouteTable,ArgMap,RefMap) -> Builder a -> Either Exception a
runBuilder rt m = runIdentity $ runExceptT $ runReaderT m rt

buildDecls :: ArgMap -> RefMap -> LinkNodes -> Either Exception [Decl]
buildDecls am rm lns = runBuilder ((RouteTable [] []),am,rm) $ do
                                                                  rt <- buildRT lns
                                                                  local (const (rt,am,rm)) constructDecls

iAnchors :: Node -> Builder [IAnchor]
iAnchors n = do
               (_,am,_) <- ask
               let nid = nodeId n
                   nr  = nodeRef n
                   anks = getAnchors am nr
               return $ map (\i -> IAnchor nid i) anks 

getNodes :: Builder [Node]
getNodes = do (rm,_,_) <- ask 
              return $ nodes rm

getLinks :: Builder [Link]
getLinks = do (rm,_,_) <- ask
              return $ links rm

getNode :: NodeId -> Builder Node
getNode id = do
                    ns <- getNodes
                    liftMaybe ("Missing node identifier") $ find (finder id) ns


getIAnchors :: NodeId -> Builder [IAnchor] 
getIAnchors id = do
                   ns <- getNodes
                   node <- liftMaybe ("Missing ianchor/node") $ find (finder id) ns
                   iAnchors node

getOAnchor :: NodeId -> Builder OAnchor
getOAnchor id = do
                   ns <- getNodes
                   node <- liftMaybe ("Missing oanchor/node") $ find (finder id) ns
                   return $ oAnchor node

getFuns :: Builder [Node]
getFuns = liftM (filter isPureFun) getNodes

getDevs :: Builder [Node]
getDevs = liftM (filter isDev) getNodes

getInLink :: IAnchor -> Builder (OAnchor,IAnchor)
getInLink i = do
               ls <- getLinks
               liftMaybe ("Unable to find link for anchor: " ++ show i) $ find (\(_,inp) -> inp == i) ls

getOutLink :: OAnchor -> Builder (OAnchor,IAnchor)
getOutLink o = do
                ls <- getLinks
                liftMaybe ("Unable to find link for anchor: " ++ show o) $ find (\(out,_) -> out == o) ls

--Checks if all nodes are completely linked up.
--Will stop process with exception if not
fullyApplied :: Node -> Builder ()
fullyApplied n = do
                   let nid = nodeId n
                   ianchors <- getIAnchors nid
                   oanchor <- getOAnchor nid
                   mapM_ getInLink ianchors
                   getOutLink oanchor
                   return ()

validInput :: Builder ()
validInput = do
               ls <- getLinks 
               liftMaybe ("Input is unconnected.") $ find (\(inp,_) -> inp == OAnchor Input) ls -- at least one Input connection
               return ()

validOutput :: Builder ()
validOutput = do
               ls <- getLinks 
               let res = filter (\(_,out) -> case out of 
                                              IAnchor Output _ -> True
                                              _                -> False) ls -- only one output connection
               if length res == 1
                 then return ()
                 else throwError (E "More than one connection to output.")

validate :: Builder ()
validate = do
             validInput
             validOutput
             getNodes >>= mapM_ fullyApplied

funLets :: Exp -> Builder (Exp)
funLets exp = do
            funs <- getFuns
            decls <- mapM funToDecl funs 
            return $ letE decls exp
  where
    funToDecl (Device _ _) = lift $ throwError (E $ "Device given to funLets")
    funToDecl i@(PureFun nid nref arity) = do
                                              ias <- getIAnchors nid
                                              out <- getOAnchor nid
                                              let out' = show out
                                              srcs <- mapM getInLink ias
                                              let srcs' = map (var . name . show . fst) srcs
                                              return $ sfun noLoc 
                                                            (name out') 
                                                            []
                                                            (UnGuardedRhs $ appFun (function $ T.unpack nref) srcs')
                                                            noBinds

outputcase :: Exp -> Builder Exp
outputcase exp = do
                        nds <- getDevs
                        nds' <- mapM (getOAnchor .  nodeId) nds
                        let pats = map (pvar . name . show) nds' 
                        return $ caseE (var $ name "output") [alt noLoc (nestedTupleP pats) exp]

inputexpr :: Builder Exp
inputexpr = do
              devs <- getDevs
              anks <- liftM concat $ mapM (getIAnchors . nodeId) devs
              outs <- mapM getInLink anks
              let outs' = map (var . name . show . fst) outs
              return $ nestedTuple outs' 

pipe_fun :: Builder Decl
pipe_fun = do
               rhs_expr <- inputexpr >>= funLets >>= outputcase 
               return $ sfun
                              noLoc
                              (name "pipe")
                              [(name "output"), (name "input")]
                              (UnGuardedRhs rhs_expr)
                              noBinds

outputexpr :: Builder Exp
outputexpr = do
               lns <- getLinks
               (res,_)  <- liftMaybe "No output link" (find isOutputLink lns)
               return $ (var . name . show) res


  where
    isOutputLink (_,IAnchor Output _) = True
    isOutputLink _ = False

out_fun :: Builder Decl
out_fun = do
               rhs_expr <- outputexpr >>= funLets >>= outputcase 
               return $ sfun
                              noLoc
                              (name "out")
                              [(name "output")]
                              (UnGuardedRhs rhs_expr)
                              noBinds
parallelExpr :: Builder Exp
parallelExpr = do 
                 devs <- getDevs
                 let devRefs = map (\(Device _ ref) -> ref) devs 
                 return $ nestedDevRef (map T.unpack devRefs)
   where
     nestedDevRef [] = error "nestedDevRef encountered empty list."
     nestedDevRef (e:[]) = (var . name) e
     nestedDevRef (e:es) = appFun (function "parI") ((var . name) e : [nestedDevRef es])

startDecl :: Builder Decl
startDecl = do
              exp <- parallelExpr
              return $ sfun noLoc
                            (name "start")
                            []
                            (UnGuardedRhs $ appFun (function "refold") [var $ name "out", var $ name "pipe", exp])
                            noBinds

constructDecls :: Builder [Decl]
constructDecls = do
                   validate
                   lns <- getLinks
                   nds <- getNodes
                   nds' <- filterM noFunInputDep nds
                   (_,am,rm) <- ask
                   outp <- local (const (RouteTable nds' lns,am,rm)) out_fun
                   dec <- pipe_fun
                   start <- startDecl
                   return [dec,outp,start]
  where
    noFunInputDep (PureFun nid nref indx) = do
                                               lins <- getIAnchors nid >>= mapM getInLink
                                               return $ all (not . isInputDep) lins  --Return True if the function isn't input-dependent
    noFunInputDep _ = return True 

    isInputDep (OAnchor Input,_) = True
    isInputDep _ = False




                 
 
nestedTuple :: [Exp] -> Exp
nestedTuple [] = error "nestedTuple encountered an empty list."
nestedTuple (e:[]) = e
nestedTuple (e:es) = tuple (e : [nestedTuple es])

nestedTupleP :: [Pat] -> Pat
nestedTupleP [] = error "nestedTupleP encountered an empty list."
nestedTupleP (p:[]) = p
nestedTupleP (p:ps) = pTuple (p : [nestedTupleP ps])
