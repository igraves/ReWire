{-# LANGUAGE GADTs #-}
module ReWire.Modular where

--Modules have a name, import references, exports, and a collection of entities, 
data Module m s where
  Module :: (Ord m, Ord s) => m -> [Entity m s] -> Module m s 

--Entities are parameterized over module and symbol types, resp
--Entities have a name, a list of all entity references they depend on, and their corresponding module reference
newtype Entity m s = Symbol (s,[s], m)

