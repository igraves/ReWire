{- LANGUAGE OverloadedStrings -}
module ReWire.Core.Transformations.Qualify where


import Control.Monad
import Control.Monad.Reader
import Control.Applicative
import Data.ByteString.Char8

import qualified Data.ByteString.Char8 as BS

import ReWire.Scoping
import ReWire.Core.Syntax
import ReWire.Core.Transformations.Uniquify 
import ReWire.Core.Transformations.DeUniquify


type QM = Reader String


pref :: QM String
pref = ask

prefBS :: QM ByteString
prefBS = liftM pack ask

qual :: String -> QM String
qual s = liftM (++ s) pref

qualBS :: ByteString -> QM ByteString
qualBS s = liftM (`append` s) prefBS

rV ::  Id a -> QM (Id a)
rV i@(Id k n) = if BS.elem '@' n
                 then return i
                 else do
                        n' <- qualBS n
                        return $ Id k n'

qT :: RWCTy -> QM RWCTy
qT ty = do
                pref <- pref
                case ty of
                  (RWCTyCon (TyConId s)) -> liftM (RWCTyCon . TyConId) (qual s) 
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
                    RWCCase exp alts -> undefined

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
