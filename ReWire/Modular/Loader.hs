{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module ReWire.Modular.Loader where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.List
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Either
import System.Directory

import ReWire.Scoping (Id(..))
import ReWire.Core.Parser
import ReWire.Core.Syntax
import ReWire.Core.Transformations.Qualify hiding (Map)
import ReWire.Core.Transformations.Mangle

import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Char8 as BS

import qualified Data.Traversable

type Map = M.Map
type LoadedModules = S.Set ModuleName
type RWL a = StateT (FilePath, LoadedModules, RWCProg) ((ExceptT String) IO) a

withEither :: MonadError e z => Either e a -> z a
withEither (Right a) = return a 
withEither (Left  e) = throwError e

prefEither :: MonadError String z => String -> Either String a -> z a
prefEither _   (Right a) = return a
prefEither str (Left e)  = throwError (str ++ e)

--SHAMELESSLY STOLEN FROM: http://stackoverflow.com/questions/19895930/using-monadic-functions-with-data-map-fx-unionwith
unionWithM :: (Monad m, Ord k) => (a -> a -> m a) -> Map k a -> Map k a -> m (Map k a)
unionWithM f mapA mapB = Data.Traversable.sequence $ M.unionWith (\a b -> do {x <- a; y <- b; f x y}) (M.map return mapA) (M.map return mapB)

unionWithKeyM :: (Monad m, Ord k) => (k -> a -> a -> m a) -> Map k a -> Map k a -> m (Map k a)
unionWithKeyM f mapA mapB = Data.Traversable.sequence $ M.unionWithKey (\k a b -> do {x <- a; y <- b; f k x y}) (M.map return mapA) (M.map return mapB)

basePath :: RWL FilePath
basePath = liftM (\(x,_,_) -> x) get

isLoaded :: ModuleName -> RWL Bool
isLoaded m = do 
               s <- liftM (\(_,x,_) -> x) get
               return $ S.member m s

addLoaded :: ModuleName -> RWL ()
addLoaded m = do
                (f,s,p) <- get
                put (f,S.insert m s,p)

getProg :: RWL RWCProg
getProg = liftM (\(_,_,p) -> p) get

putProg :: RWCProg -> RWL ()
putProg p = do
             (a,b,_) <- get
             put (a,b,p)

procMod :: ModuleName -> RWL ([ImportName])
procMod m = do
              l <- isLoaded m
              case l of
                  True  -> return []
                  False -> do
                             main <- getProg
                             prog <- loadModule m
                             (term_names, type_names) <- modNS prog
                             let main' = (qualify term_names type_names prog) >: main
                             addLoaded m
                             putProg main'
                             return (map impName $ imports prog)

idName :: Id a -> ByteString
idName (Id _ n) = n

modNS :: RWCProg -> RWL (Map ByteString ByteString,Map ByteString ByteString)
modNS prog = do
               let imps = imports prog
               maps <- mapM importNS imps
               let term_maps = map fst maps
                   type_maps = map snd maps
               term_map <- foldM (\acc item -> unionWithKeyM uf acc item) M.empty term_maps 
               type_map <- foldM (\acc item -> unionWithKeyM uf acc item) M.empty type_maps 
               return (term_map,type_map)
  where
    uf key v1 v2 = if v1 /= v2
                    then throwError $ "Ambiguous name: " ++ (BS.unpack key)
                    else return v1
                
               

importNS :: Import -> RWL (Map ByteString ByteString,Map ByteString ByteString)
importNS im = do
              term_names <- moduleTermNames (impName im)
              type_names <- moduleTypeNames (impName im)
              case im of
                Qualified mn -> do
                                  let term_names' = prefNames mn term_names
                                      type_names' = prefNames mn type_names
                                  return $ (M.fromList $ zip term_names' term_names', M.fromList $ zip type_names' type_names')
              
                QualifiedAs mn as_ -> do
                                        let term_val_names = prefNames mn  term_names
                                            term_key_names = prefNames as_ term_names
                                            type_val_names = prefNames mn  type_names
                                            type_key_names = prefNames as_ type_names
                                        return $ (M.fromList $ zip term_key_names term_val_names,
                                                  M.fromList $ zip type_key_names type_val_names)
                Unqualified mn -> do
                                    let term_qual_names = prefNames mn term_names
                                        term_namelist   = zip term_names term_qual_names ++ zip term_qual_names term_qual_names
                                        type_qual_names = prefNames mn type_names
                                        type_namelist   = zip type_names type_qual_names ++ zip type_qual_names type_qual_names
                                    return $ (M.fromList term_namelist, M.fromList type_namelist)
                UnqualifiedAs mn as_ -> do
                                    let term_qual_names = prefNames as_ term_names
                                        term_val_names  = prefNames mn term_names
                                        term_namelist = zip term_names term_val_names ++ zip term_qual_names term_val_names
                                        type_qual_names = prefNames as_ type_names
                                        type_val_names  = prefNames mn type_names
                                        type_namelist = zip type_names type_val_names ++ zip type_qual_names type_val_names
                                    return $ (M.fromList term_namelist,M.fromList type_namelist)
  where
    prefNames :: ByteString -> [ByteString] -> [ByteString]
    prefNames pref names = map (pref `BS.append` "." `BS.append`) names

moduleTermNames :: ModuleName -> RWL [ByteString]
moduleTermNames mn = do
                   prog <- loadModule mn
                   let defs = map (idName . defnName) $ defns prog 
                       cons = concatMap getCons $ dataDecls prog
                   return (defs ++ cons) 
  where
    getCons x = map (BS.pack . deDataConId . (\(RWCDataCon d _) -> d)) $ dataCons x

moduleTypeNames :: ModuleName -> RWL [ByteString]
moduleTypeNames mn = do
                   prog <- loadModule mn
                   let tys  = map getTys $ dataDecls prog
                   return (tys) 
  where
    getTys = (BS.pack . deTyConId . dataName) 
                   

loadModule :: ModuleName -> RWL RWCProg
loadModule m = do 
                 let um = (unpack m)
                 base <- basePath
                 let m' = base ++ "/" ++ (map (\x -> if x == '.' then '/' else x) um) ++ ".rwc"
                 f <- liftIO $ doesFileExist m'
                 case f of
                    False -> throwError $ "Module " ++ um ++ ": " ++ "Missing file.  Checked for " ++ m'
                    True  -> do
                               pm <- liftIO $ parsefile m'
                               pf <- prefEither ("Module " ++ um ++ ": " ++ "Failed to parse. ") pm
                               case liftM (m ==) $ modname pf of
                                            Nothing    -> throwError $ "Module " ++ um ++ ": File matches name, but module does not contain a module header."
                                            Just False -> throwError $ "Module " ++ um ++ ": File matches name, but module has differing name in header."
                                            _          -> return pf 
--RWCProg merging function.
--Right argument retains name.
(>:) :: RWCProg -> RWCProg -> RWCProg
(RWCProg ln lims ldecs lprims ldefs) >: (RWCProg mn rims rdecs rprims rdefs) = let ims'  = case ln of
                                                                                            Nothing -> rims
                                                                                            Just ln -> filter (\x -> case x of 
                                                                                                                         Qualified x -> x /= ln
                                                                                                                         _ -> True) rims
                                                                                   ims'' = unionBy (==) lims rims
                                                                                   decs  = unionBy (\x y -> dataName x == dataName y) ldecs rdecs
                                                                                   prims = unionBy (\x y -> primName x == primName y) lprims rprims
                                                                                   defs  = unionBy (\x y -> defnName x == defnName y) ldefs rdefs
                                                                             in RWCProg mn ims'' decs prims defs

loadMods :: [ModuleName] -> RWL () 
loadMods mods = do
                  case mods of
                      [] -> return ()
                      ms -> do
                              mms <- mapM procMod mods
                              let mms' = concat mms
                              loadMods mms'

loadImports :: FilePath -> RWCProg -> IO (Either String RWCProg)
loadImports path prog = do 
                            let exp = runStateT (do
                                                    prog <- getProg
                                                    (terms,types) <- modNS prog
                                                    let prog' = qualify_ terms types prog
                                                    putProg prog'
                                                    loadMods (map impName $ imports prog')) (path, S.empty, prog)
                            res <- runExceptT exp
                            case res of
                               (Left s) -> return (Left s)
                               (Right ((), (_,_,prog))) -> return (Right $ mangle mangler prog)
                                          
