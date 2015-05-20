{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module ReWire.Modular.Types where
import Data.Functor
import Control.Applicative
import Control.Monad
import Data.Functor.Identity
import qualified Data.ByteString as BS

type ByteString = BS.ByteString
type EntityKey  = ByteString
type ModuleKey  = ByteString
type Imports    = [ModuleKey]
type Exports    = [EntityKey]

type ReWireEntity = ()

--Modules have a name, import references, exports, and a collection of entities, 
data Module a = Module ModuleKey Imports Exports [Entity a]
data Entity a = Entity EntityKey a 

--Providers of modules (by scanning directories, etc.)
--All names are fully qualified
class (Monad m) => ModuleProvider m where
  --Get all available modules loaded in the module provider
  getModules     :: m [Module ReWireEntity]
  --Get a module by its name
  getModule      :: ModuleKey -> m [Module]
  --Get the local entity names in a module 
  getLocalNames  :: ModuleKey -> m [EntityKey]
  --Get the dependencies of an entity
  getDeps        :: EntityKey -> m [EntityKey]



