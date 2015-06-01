{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module ReWire.Core.Transformations.Mangle where


import Control.Monad
import Control.Monad.Reader
import Control.Applicative
--import Data.ByteString.Char8 hiding (elem)
import Data.Char
import Data.String

import qualified Data.ByteString.Char8 as BS

import ReWire.Scoping
import ReWire.Core.Syntax
import ReWire.Core.Transformations.Uniquify 
import ReWire.Core.Transformations.DeUniquify

import Data.List (foldl')

cmdMangle _ prog = (Just (mangle mangler prog),Nothing)

mangler :: String -> String
mangler = dots 
  where
    dots = foldl' (\acc item -> case item of
                                    '.' -> acc ++ "dot"
                                    a   -> acc ++ [a]) []

mangle :: (String -> String) -> RWCProg -> RWCProg
mangle f prog = qProg f prog

type ByteString = BS.ByteString
class WithString a where
  withString :: (String -> String) -> a -> a

instance WithString ByteString where
  withString f = BS.pack . f . BS.unpack 

instance WithString [Char] where
  withString f = f
  
  


rV :: (String -> String) -> Id a -> Id a
rV f i@(Id k n) = if BS.elem '@' n
                   then i
                   else let n' = withString f n 
                         in Id k n'
qT :: (String -> String) -> RWCTy -> RWCTy
qT f ty = case ty of
              (RWCTyCon (TyConId s)) -> (RWCTyCon . TyConId) (withString f s) 
              (RWCTyApp t1 t2)       -> RWCTyApp (qT f t1) (qT f t2)
              t@(RWCTyVar _)         -> t
              (RWCTyComp t1 t2)      -> RWCTyComp (qT f t1) (qT f t2)

qPTy :: (String -> String) -> Poly RWCTy -> (Poly RWCTy)
qPTy f (ts :-> t) = let ts' = map (rV f) ts
                        t'  = qT f t
                     in (ts' :-> t')


qE :: (String -> String) -> RWCExp -> RWCExp
qE f exp = case exp of
                    RWCApp e1 e2     -> RWCApp (qE f e1) (qE f e2)
                    RWCLam i ty exp  -> (RWCLam i) (qT f ty) (qE f exp)
                    RWCLet i e1 e2   -> (RWCLet i) (qE f e1) (qE f e2) 
                    RWCVar n ty      -> RWCVar (rV f n) (qT f ty)
                    RWCCon (DataConId n) ty -> RWCCon (DataConId (f n)) (qT f ty)
                    RWCLiteral lit   -> RWCLiteral lit
                    RWCCase exp alts -> RWCCase (qE f exp) (map (qA f) alts)

qP :: (String -> String) -> RWCPat -> RWCPat
qP f pat = case pat of
            RWCPatCon (DataConId n) pats -> RWCPatCon (DataConId $ f n) (map (qP f) pats)
            RWCPatVar v ty    -> (RWCPatVar v) (qT f ty)
            RWCPatLiteral lit -> RWCPatLiteral lit
            a                 -> a

qA :: (String -> String) -> RWCAlt -> RWCAlt
qA f (RWCAlt pat exp) = RWCAlt (qP f pat) (qE f exp) 

qPrim :: (String -> String) -> RWCPrim -> RWCPrim
qPrim f (RWCPrim n ty s) = RWCPrim (rV f n) (qT f ty) s

qDefn :: (String -> String) -> RWCDefn -> RWCDefn
qDefn f (RWCDefn n pty exp) = RWCDefn (rV f n) (qPTy f pty) (qE f exp)

qDataCon :: (String -> String) -> RWCDataCon -> RWCDataCon
qDataCon f (RWCDataCon (DataConId id) tys) = RWCDataCon (DataConId $ f id) (map (qT f) tys)

qData :: (String -> String) -> RWCData -> RWCData
qData f (RWCData (TyConId i) tys cons) = RWCData (TyConId $ f i) (map (rV f) tys) (map (qDataCon f) cons)
                                                
qProg :: (String -> String) -> RWCProg -> RWCProg
qProg f (RWCProg mn imp decls prims defns) = (RWCProg mn imp) (map (qData f) decls) (map (qPrim f) prims) (map (qDefn f) defns)
