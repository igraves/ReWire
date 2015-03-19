{-# LANGUAGE MultiParamTypeClasses,GeneralizedNewtypeDeriving,FlexibleInstances,DeriveGeneric #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ReWire.Core.Syntax where

import GHC.Generics (Generic,Generic1)
import Generics.Deriving.Functor
import Generics.Deriving.Foldable
import qualified Data.Foldable as Foldable
import Control.DeepSeq

type ModName = String

---

data Ty v = TyApp (Ty v) (Ty v)
          | TyVar v
          | TyComp (Ty v) (Ty v) -- application of a monad
          deriving (Ord,Eq,Show,Generic,Generic1)

instance NFData v => NFData (Ty v)
instance GFunctor Ty
instance Functor Ty where fmap = gmap
instance GFoldable Ty
instance Foldable.Foldable Ty where foldr = gfoldr

---

data PolyTy v = [v] :-> Ty v deriving (Show,Generic,Generic1)

infixr :->

instance NFData v => NFData (PolyTy v)
instance GFunctor PolyTy
instance Functor PolyTy where fmap = gmap

---

data Exp v = App (Exp v) (Exp v)
           | Lam v (Exp v)
           | Var v
           | Case (Exp v) [Alt v]
           deriving (Show,Generic,Generic1)

instance NFData v => NFData (Exp v)
instance GFunctor Exp
instance Functor Exp where fmap = gmap

---

data Alt v = Alt (Pat v) (Exp v)
           deriving (Show,Generic,Generic1)

instance NFData v => NFData (Alt v)
instance GFunctor Alt
instance Functor Alt where fmap = gmap

---

data Pat v = Pat v [Pat v]
           deriving (Show,Generic,Generic1)

instance NFData v => NFData (Pat v)
instance GFunctor Pat
instance Functor Pat where fmap = gmap

---

data Prim v = Prim { primName     :: v,
                     primTy       :: PolyTy v,
                     primVHDLName :: String }
          deriving (Show,Generic,Generic1)

instance NFData v => NFData (Prim v)
instance GFunctor Prim
instance Functor Prim where fmap = gmap

---

data Defn v = Defn { defnName   :: v,
                     defnPolyTy :: PolyTy v,
                     defnBody   :: Exp v }
            deriving (Show,Generic,Generic1)

instance NFData v => NFData (Defn v)
instance GFunctor Defn
instance Functor Defn where fmap = gmap

---

data Data v = Data { dataName   :: v,
                     dataTyVars :: [v],
                     dataCons   :: [Ctor v] }
            deriving (Show,Generic,Generic1)

instance NFData v => NFData (Data v)
instance GFunctor Data
instance Functor Data where fmap = gmap

---

data Ctor v = Ctor v [Ty v]
            deriving (Show,Generic,Generic1)

instance NFData v => NFData (Ctor v)
instance GFunctor Ctor
instance Functor Ctor where fmap = gmap

---

data Module v = Module { modName   :: ModName,
                         dataDecls :: [Data v],
                         primDecls :: [Prim v],
                         defns     :: [Defn v] }
              deriving (Show,Generic,Generic1)

instance NFData v => NFData (Module v)
instance GFunctor Module
instance Functor Module where fmap = gmap
