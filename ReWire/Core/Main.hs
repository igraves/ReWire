{-# LANGUAGE JavaScriptFFI      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE CPP                #-}

module ReWire.Core.Main where

import System.IO
import System.Environment
import ReWire.Core.Syntax
import ReWire.Core.Parser
--import ReWire.Core.PrettyPrint
import ReWire.Core.PrettyPrintHaskell
import ReWire.Core.KindChecker
import ReWire.Core.TypeChecker
import ReWire.Core.Transformations.Interactive
--GHCJS
import ReWire.PreHDL.CFG
import ReWire.PreHDL.GotoElim
import ReWire.PreHDL.ElimEmpty
import ReWire.PreHDL.ToVHDL
import ReWire.PreHDL.ConnectLogic
import ReWire.Core.Transformations.ToPreHDL

#ifdef __GHCJS__
import ReWire.RouteLogic.Types
import ReWire.RouteLogic.CodeGen
import qualified GHCJS.Types as T
import GHCJS.Marshal
import GHCJS.Foreign
import qualified Data.Text as Txt
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString.Lazy as BS

import Data.Aeson (decode')

foreign import javascript unsafe "console.log($1)" jlog :: T.JSString -> IO ()
--jlog = putStrLn
--toJSString = id
#else
jlog = putStrLn
toJSString = id
#endif

main :: IO ()
main = do args <- getArgs
          if length args /= 1
             then do n <- getProgName
                     hPutStrLn stderr $ "Syntax: " ++ n ++ " [filename.core]"
             else do let filename = head args
                     res_p <- parsefile filename
                     case res_p of
                       Left e  -> hPutStrLn stderr e
                       Right p -> do putStrLn "parse finished"
                                     writeFile "show.out" (show p)
                                     putStrLn "show out finished"
                                     writeFile "Debug.hs" (show $ ppHaskellWithName p "Debug")
                                     putStrLn "debug out finished"
                                     case kindcheck p of
                                       Just e  -> hPutStrLn stderr e
                                       Nothing -> do putStrLn "kc finished"
                                                     case typecheck p of
                                                       Left e   -> hPutStrLn stderr e
                                                       Right p' -> do putStrLn "tc finished"
                                                                      writeFile "tc.out" (show p')
                                                                      putStrLn "tc debug print finished"
                                                                      trans p'

#ifdef __GHCJS__
rwcStr :: String -> IO ()
rwcStr str = do
               let res_p = parsewithname "Visual ReWire Diagram" str 
               case res_p of
                  Left e -> jlog "Parse Failed" --Parse Failed
                  Right p -> case kindcheck p of
                                  Just e  -> jlog "Kind Check Failed" --KC Failed
                                  Nothing -> case typecheck p of
                                                  Left e   -> jlog "Typecheck failed" --Typecheck failed
                                                  Right p' -> do
                                                                --let res = clVHDL (elimEmpty $ gotoElim $ cfgToProg (cfgFromRW p'))
                                                                let res = clVHDL $ c2p p' $ cfgCLExp p'
                                                                jlog (toJSString res)
  where
    c2p p (a,b,c,d,e) = (a,b,c,map (\(x,(y,z)) -> (x,(elimEmpty $ gotoElim $ cfgToProg y,z))) d,convNCLs p e)

rewire :: T.JSRef T.JSString -> IO ()
rewire jref = do
                t <- fromJSRef jref
                case t of
                     Just jstr -> rwcStr (fromJSString jstr) 
                     Nothing   -> jlog "Couldn't unmarshal JRef."

argMap = []
refMap = []

rewireRoute :: T.JSRef T.JSString -> IO ()
rewireRoute jref = do
                     t <- fromJSRef jref
                     case t of
                        Just jstr -> do 
                                       case fromJSString jstr of
                                              str -> do
                                                         let bs  = TLE.encodeUtf8 $ TL.fromStrict str
                                                         case decode' bs of
                                                            Nothing  -> jlog "Badly formed JSON."
                                                            Just exp -> do 
                                                                          let res = buildDecls argMap refMap exp
                                                                          case res of 
                                                                              Left (E exp) -> jlog (toJSString exp)
                                                                              Right decls  -> jlog $ toJSString (show decls)
                        Nothing   -> jlog "Couldn't unmarshal JRef."
#else
rewire :: String -> IO ()
rewire = putStrLn

rewireRoute :: String -> IO ()
rewireRoute = putStrLn
#endif
