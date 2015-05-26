module ReWire.Core.Transformations.Qualify where


import Control.Monad
import Control.Monad.Reader
import Control.Applicative
import Data.ByteString.Char8

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

qT :: RWCTy -> QM RWCTy
qT ty = do
                pref <- pref
                case ty of
                  (RWCTyCon (TyConId s)) -> liftM (RWCTyCon . TyConId) (qual s) 
                  (RWCTyApp t1 t2)       -> liftM2 RWCTyApp (qT t1) (qT t2)
                  t@(RWCTyVar _)         -> return t
                  (RWCTyComp t1 t2)      -> liftM2 RWCTyComp (qT t1) (qT t2)

qE :: RWCExp -> QM RWCExp
qE exp = case exp of
                    RWCApp e1 e2 -> liftM2 RWCApp (qE e1) (qE e2)
                    RWCLam 
