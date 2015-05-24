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

import Data.ByteString.Char8 (unpack)

type LoadedModules = S.Set ModuleName
type RWL a = StateT (FilePath, LoadedModules) ((ExceptT String) IO) a

withEither :: MonadError e z => Either e a -> z a
withEither (Right a) = return a 
withEither (Left  e) = throwError e

prefEither :: MonadError String z => String -> Either String a -> z a
prefEither _   (Right a) = return a
prefEither str (Left e)  = throwError (str ++ e)

basePath :: RWL FilePath
basePath = liftM fst get

isLoaded :: ModuleName -> RWL Bool
isLoaded m = do 
              s <- liftM snd get
              return $ S.member m s

addLoaded :: ModuleName -> RWL ()
addLoaded m = do
                (f,s) <- get
                put (f,S.insert m s)

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



loadMerge :: ModuleName -> RWCProg -> RWL RWCProg
loadMerge = undefined
