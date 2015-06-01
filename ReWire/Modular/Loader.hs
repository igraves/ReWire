{-# LANGUAGE FlexibleContexts #-}

module ReWire.Modular.Loader where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.List
import qualified Data.Set as S
import Data.Either
import System.Directory

import ReWire.Core.Parser
import ReWire.Core.Syntax
import ReWire.Core.Transformations.Qualify
import ReWire.Core.Transformations.Mangle

import Data.ByteString.Char8 (unpack)

type LoadedModules = S.Set ModuleName
type RWL a = StateT (FilePath, LoadedModules, RWCProg) ((ExceptT String) IO) a

withEither :: MonadError e z => Either e a -> z a
withEither (Right a) = return a 
withEither (Left  e) = throwError e

prefEither :: MonadError String z => String -> Either String a -> z a
prefEither _   (Right a) = return a
prefEither str (Left e)  = throwError (str ++ e)

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
                             let main' = (qualify prog) >: main
                             addLoaded m
                             putProg main'
                             return (imports prog)


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
                                                                                            Just ln -> filter (\x -> x /= ln) rims
                                                                                   ims'' = unionBy (==) lims rims
                                                                                   decs  = unionBy (\x y -> dataName x == dataName y) ldecs rdecs
                                                                                   prims = unionBy (\x y -> primName x == primName y) lprims rprims
                                                                                   defs  = unionBy (\x y -> defnName x == defnName y) ldefs rdefs
                                                                             in RWCProg mn ims'' decs prims defs

loadMods :: [ModuleName] -> RWL () 
loadMods mods = case mods of
                      [] -> return ()
                      ms -> do
                              mms <- mapM procMod mods
                              let mms' = concat mms
                              loadMods mms'

loadImports :: FilePath -> RWCProg -> IO (Either String RWCProg)
loadImports path prog = do 
                            let exp = runStateT (loadMods (imports prog)) (path, S.empty, prog)
                            res <- runExceptT exp
                            case res of
                               (Left s) -> return (Left s)
                               (Right ((), (_,_,prog))) -> return (Right $ mangle mangler prog)
                                          
