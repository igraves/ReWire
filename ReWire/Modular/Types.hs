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

--Modules have a name, import references, exports, and a collection of entities, 
data Module m s where
  Module :: (Ord m, Ord s) => m -> [m] -> [s] -> [Entity m s] -> Module m s 

--Entities are parameterized over module and symbol types, resp
--Entities have a name, a list of all entity references they depend on, and their corresponding module reference
newtype Entity m s = Symbol (s,[s], m)

--Providers of modules (by scanning directories, etc.)
class (Monad m) => ModuleProvider m where
  getModules :: m [Module ByteString ByteString]

