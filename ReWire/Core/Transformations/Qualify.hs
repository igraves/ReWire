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

{-
qualifyTy :: RWCTy -> RWCTy
qualifyTy ty = case ty of
                  (RWCTyCon 
                  (RWCTyApp t1 t2) -> RWCTyApp (qualifyTy t1) (qualifyTy t2)
-}