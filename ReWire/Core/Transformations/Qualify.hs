{-# LANGUAGE OverloadedStrings #-}
module ReWire.Core.Transformations.Qualify where


import Control.Monad
import Control.Monad.Reader
import Control.Applicative
import Data.ByteString.Char8 hiding (elem)
import Data.Char
import Data.String

import qualified Data.Map.Strict as M

import qualified Data.ByteString.Char8 as BS

import ReWire.Scoping
import ReWire.Core.Syntax
import ReWire.Core.Transformations.Uniquify 
import ReWire.Core.Transformations.DeUniquify

type Map = M.Map

cmdQualify _ prog = (Nothing,Just "Qualify disabled atm.")

qualify :: Map ByteString ByteString -> Map ByteString ByteString -> RWCProg -> RWCProg
qualify terms types prog = case modname prog of
                     Nothing -> error "Qualifying module with no module name"
                     Just mn -> let --mn' = (BS.unpack mn) ++ (fromString ".")
                                    (prog',_) = uniquify 0 prog 
                                    prog'' = runReader (qProg prog) ((terms,types),mn,True)
                                 in deUniquify prog''
--No local renaming
qualify_ :: Map ByteString ByteString -> Map ByteString ByteString -> RWCProg -> RWCProg
qualify_ terms types prog = case modname prog of
                     Nothing -> error "Qualifying module with no module name"
                     Just mn -> let --mn' = (BS.unpack mn) ++ (fromString ".")
                                    (prog',_) = uniquify 0 prog 
                                    prog'' = runReader (qProg prog) ((terms,types),mn,False)
                                 in deUniquify prog''

type QM = Reader ((Map ByteString ByteString,Map ByteString ByteString),ByteString,Bool)


--TODO: This could be made stronger, but this may do well enough.
isQStr :: String -> Bool
isQStr (s:str) = isUpper s && '.' `elem` str

isQBS :: ByteString -> Bool
isQBS bs = case BS.length bs > 0 of
                True -> (isUpper . BS.head) bs && BS.elem '.' bs
                False -> False

qual :: String -> QM String
qual s = do
           let s' = BS.pack s
           ((mp,_),pre,loc) <- ask
           case M.lookup s' mp of
                Nothing  -> if loc 
                              then return $ (BS.unpack pre) ++ "." ++ s
                              else return s
                Just ql  -> return (BS.unpack ql)


qualBS :: ByteString -> QM ByteString
qualBS s = do 
           ((mp,_),pre,loc) <- ask
           case M.lookup s mp of
                Nothing  -> if loc
                             then return $ pre `BS.append` "." `BS.append` s 
                             else return s
                Just ql  -> return ql

qualTy :: String -> QM String
qualTy s = do
           let s' = BS.pack s
           ((_,mp),pre,loc) <- ask
           case M.lookup s' mp of
                Nothing  -> if loc 
                              then return $ (BS.unpack pre) ++ "." ++ s
                              else return s
                Just ql  -> return (BS.unpack ql)

qualTyBS :: ByteString -> QM ByteString
qualTyBS s = do
               ((_,mp),pre,loc) <- ask
               case M.lookup s mp of
                    Nothing  -> if loc
                                 then return $ pre `BS.append` "." `BS.append` s 
                                 else return s
                    Just ql  -> return ql
               

rV ::  Id a -> QM (Id a)
rV i@(Id k n) = if BS.elem '@' n
                 then return i
                 else do
                        n' <- qualBS n
                        return $ Id k n'

qT :: RWCTy -> QM RWCTy
qT ty = do
                pref <- liftM (BS.unpack . (\(_,x,_) -> x)) ask 
                case ty of
                  arr@(RWCTyCon (TyConId s)) -> if s == "(->)"
                                             then return arr
                                             else liftM (RWCTyCon . TyConId) (qualTy s) 
                  (RWCTyApp t1 t2)       -> liftM2 RWCTyApp (qT t1) (qT t2)
                  t@(RWCTyVar _)         -> return t
                  (RWCTyComp t1 t2)      -> liftM2 RWCTyComp (qT t1) (qT t2)

qPTy :: Poly RWCTy -> QM (Poly RWCTy)
qPTy (ts :-> t) = do
                    ts' <- mapM rV ts
                    t'  <- qT t
                    return (ts' :-> t')

qE :: RWCExp -> QM RWCExp
qE exp = case exp of
                    RWCApp e1 e2     -> liftM2 RWCApp (qE e1) (qE e2)
                    RWCLam i ty exp  -> liftM2 (RWCLam i) (qT ty) (qE exp)
                    RWCLet i e1 e2   -> liftM2 (RWCLet i) (qE e1) (qE e2) 
                    RWCVar n ty      -> liftM2 RWCVar (rV n) (qT ty)
                    RWCCon (DataConId n) ty -> liftM2 RWCCon (liftM DataConId (qual n)) (qT ty)
                    RWCLiteral lit   -> return $ RWCLiteral lit
                    RWCCase exp alts -> liftM2 RWCCase (qE exp) (mapM qA alts)

qP :: RWCPat -> QM RWCPat
qP pat = case pat of
            RWCPatCon (DataConId n) pats -> liftM2 RWCPatCon (liftM DataConId $ qual n) (mapM qP pats)
            RWCPatVar v ty    -> liftM (RWCPatVar v) (qT ty)
            --RWCPatLiteral lit -> return $ RWCPatLiteral lit
            a                 -> return a

qA :: RWCAlt -> QM RWCAlt
qA (RWCAlt pat exp) = liftM2 RWCAlt (qP pat) (qE exp) 

qPrim :: RWCPrim -> QM RWCPrim
qPrim (RWCPrim n ty s) = liftM3 RWCPrim (rV n) (qT ty) (return s)

qDefn :: RWCDefn -> QM RWCDefn
qDefn (RWCDefn n pty exp) = liftM3 RWCDefn (rV n) (qPTy pty) (qE exp)

qDataCon :: RWCDataCon -> QM RWCDataCon
qDataCon (RWCDataCon (DataConId id) tys) = liftM2 RWCDataCon (liftM DataConId $ qual id) (mapM qT tys)

qData :: RWCData -> QM RWCData
qData (RWCData (TyConId i) tys cons) = liftM3 RWCData (liftM TyConId $ qual i) (mapM rV tys) (mapM qDataCon cons)
                                                
qProg :: RWCProg -> QM RWCProg
qProg (RWCProg mn imp decls prims defns) = liftM3 (RWCProg mn imp) (mapM qData decls) (mapM qPrim prims) (mapM qDefn defns)
