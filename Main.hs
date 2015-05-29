module Main (main, rewire) where

import qualified ReWire.Core.Main as M 

main = M.main

--ReWire for export in JS.  Kind of a hack!
rewire = rewire
