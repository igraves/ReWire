module ReWire.Core.Rename where

import ReWire.Core.Syntax
import Data.Functor.Identity

data Name = ExpName String (PolyTy Name)
          | ExpCon String (PolyTy Name)
          | TyName String (Kind Name)
          | TyCon String (Kind Name)
          | TcName String (Kind Name)
          | KcName String
          deriving (Eq,Show)

type RM     = ReaderT [Assump] (StateT Int Identity)
type Assump = (String,Name)

rnData :: Data String -> RM ([Assump],Data Name)
rnData (Data v tvs ctors) = do
  kcns       <- mapM (const freshKcName) tvs
  let ks     =  map KiVar kcns
      tns    =  zipWith TyName tvs ks
      as     =  zip tvs tns

  let n      =  TyCon v (foldr KiArrow KiStar ks)
      as'    =  (v,n):as

  a_ctors    <- assuming as' (mapM (rnCtor n tns) ctors)
  let as''   =  map fst a_ctors ++ as'
      ctors' =  map snd ctors
  
  return (as'',Data n tns ctors')

rnCtor :: Name -> [Name] -> Ctor String -> RM (Assump,Ctor Name)
rnCtor ncon ns (Ctor v tys) = do
  tys'   <- mapM rnTy tys
  let tr =  foldr mkArrow (foldl TyApp (TyVar ncon) (map TyVar ns)) tys'
      n  =  ExpCon v (mkPoly tr)
  return ((v,n),Ctor n tys')

rnTy :: Ty String -> RM (Ty Name)
rnTy (TyApp t1 t2)   = do t1' <- rnTy t1
                          t2' <- rnTy t2
                          return (TyApp t1' t2')
rnTy (TyVar x)       = do mn <- askAbout x
                          case mn of
                           Just n  -> return (TyVar n)
                           Nothing -> throwError $ "Unbound type variable: " ++ x
rnTy (TyComp t1 t2) = do t1' <- rnTy t1
                         t2' <- rnTy t2
                         return (TyComp t1' t2')

rmPrim :: Prim String -> RM (Assump,Prim Name)
rmPrim (Prim v pt vhdlname) = do
  pt'   <- rnPolyTy pt
  let n =  ExpName v pt'
  return ((v,n),Prim n pt')

{-
rnModule :: Module String -> RM (Module Name)
rnModule (Module n dds pds defns) = do
  dds'  <- mapM rnData dds
  pds'  <- mapM rnPrim pds
  defns <- mapM rnDefn defns-}
