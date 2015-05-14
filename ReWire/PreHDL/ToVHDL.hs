{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module ReWire.PreHDL.ToVHDL where

import ReWire.PreHDL.Syntax
import ReWire.PreHDL.ConnectLogic
import Data.List (intercalate)
import qualified Data.Map.Strict as Map

import Debug.Trace

vTy (TyBits n) = "std_logic_vector(0 to " ++ show (n-1) ++ ")"
vTy TyBoolean  = "boolean"

vInit (TyBits _) = "(others => '0')"
vInit TyBoolean  = "false"

vRegDecl rd = "variable " ++ regDeclName rd ++ " : " ++ vTy (regDefnTy rd) ++ " := " ++ vInit (regDefnTy rd) ++ ";"

vHeader :: Header -> String
vHeader h = concatMap ((++"\n") . vRegDecl) (regDecls h) -- FIXME: fundefns, state names, start state

vBool :: BoolExp -> String
vBool (And b1 b2)        = "(" ++ vBool b1 ++ " AND " ++ vBool b2 ++ ")"
vBool (Or b1 b2)         = "(" ++ vBool b1 ++ " OR " ++ vBool b2 ++ ")"
vBool (Not b)            = "(NOT " ++ vBool b ++ ")"
vBool (BoolVar l)        = l
vBool (BoolConst True)   = "true"
vBool (BoolConst False)  = "false"
vBool (InState n)        = "(control = STATE" ++ show n ++ ")"
vBool (BoolEq rhs1 rhs2) = "(" ++ vRHS rhs1 ++ " = " ++ vRHS rhs2 ++ ")"

vRHS :: RHS -> String
vRHS (BoolRHS b)        = vBool b
vRHS (LocRHS "input")   = "input_tmp" -- FIXME: kludge
vRHS (LocRHS l)         = l
vRHS (FunCallRHS s [])  = s
vRHS (FunCallRHS s ls)  = s ++ "(" ++ intercalate "," ls ++ ")"
vRHS (ConstRHS bs)      = "\"" ++ concatMap show bs ++ "\""
vRHS (SliceRHS lo hi r) = r ++ "(" ++ show lo ++ " to " ++ show hi ++ ")"
vRHS (ConcatRHS ls)     = "(" ++ intercalate " & " ls ++ ")"

vCmd :: Cmd -> String
vCmd (Rem c)        = "-- " ++ c
vCmd (Assign "output" rhs) = "output_tmp := " ++ vRHS rhs ++ ";" -- FIXME; kludge :/
--vCmd (Assign "output" rhs) = "nextout := " ++ vRHS rhs ++ ";" -- FIXME; kludge :/
vCmd (Assign l rhs) = l ++ " := " ++ vRHS rhs ++ ";"
vCmd (NextState n)  = "control := STATE" ++ show n ++ ";"
vCmd (If b c)       = "if " ++ vBool b ++ " then\n"
                   ++ indent (vCmd c) ++ "\n"
                   ++ "end if;"
vCmd (Seq c1 c2)    = vCmd c1 ++ "\n" ++ vCmd c2
vCmd Skip           = "null;"
vCmd (Goto _ _)     = error "vCmd: encountered a goto"
vCmd (Lbl l)        = "null; -- label " ++ l

vFunDefnProto :: FunDefn -> String
vFunDefnProto fd = "function " ++ funDefnName fd ++ (if null params
                                                        then ""
                                                        else "(" ++ intercalate " ; " (map ((++" : std_logic_vector") . regDeclName) params) ++ ")") 
                                                 ++ " return std_logic_vector;"
                   where params = funDefnParams fd

vFunDefn :: FunDefn -> String
vFunDefn fd = "function " ++ funDefnName fd ++ (if null params
                                                   then ""
                                                   else "(" ++ intercalate " ; " (map ((++" : std_logic_vector") . regDeclName) params) ++ ")") 
                                            ++ " return std_logic_vector\n"
           ++ "is\n"
           ++ indent (concatMap ((++"\n") . vRegDecl) (funDefnRegDecls fd))
           ++ "begin\n"
           ++ indent (vCmd (funDefnBody fd) ++ "\n")
           ++ indent ("return " ++ funDefnResultReg fd ++ ";\n")
           ++ "end " ++ funDefnName fd ++ ";"
      where params = funDefnParams fd

flopName n = n ++ "_flop"
flopNextName n = n ++ "_flop_next"

--toVHDL :: Prog -> String
--toVHDL p = "library ieee;\n"
--        ++ "use ieee.std_logic_1164.all;\n"
--        ++ "-- Comment out the following line if VHDL primitives are not in use.\n"
--        ++ "use work.prims.all;\n"
--        ++ "entity rewire is\n"
clVHDL :: (String,(Int,Int),Map.Map String (Int,Int),[(String, (Prog, (Int, Int)))],[NCLF]) -> String
clVHDL (main_is,(mi,mo),iomap, ps, named) = let entities = concatMap (\(s,(p,_)) -> toVHDL s p) ps
                                          in entities
                                             ++ (concatMap (procCL iomap) named)
                                             ++ main mi mo main_is

progVHDL :: [(String,(Prog,(Int,Int)))] -> String
progVHDL ps = let entities = concatMap (\(s,(p,_)) -> toVHDL s p) ps
                  iwidth   = foldr (\(_,(_,(i,_))) acc -> i+acc) 0 ps
                  owidth   = foldr (\(_,(_,(_,o))) acc -> o+acc) 0 ps
            in entities -- ++ main iwidth owidth ps

procCL :: Map.Map String (Int,Int) -> NCLF -> String
procCL m (n,(Par devs)) = let devs' = map devRefs devs 
                              (i,o) = case Map.lookup n m of
                                            Nothing -> error $ "procCL: Encountered an unknown reference (non-leaf).  For: " ++ (show n)
                                            Just z  -> z
                           in pars i o n devs'
  where
    devRefs :: CLFNamed -> (String,(Int,Int))
    devRefs (Leaf s) = case Map.lookup s m of
                              Nothing -> error "procCL: Encountered an unknown Leaf reference"
                              Just z  -> (s,z)
    devRefs _        = error "procCL: devRefs encountered a non-Leaf"





procCL m (n,(ReFold f1 f2 (Leaf dev))) = let (i,o) = case Map.lookup n m of
                                                               Nothing -> error "procCL: Encountered an unknown reference on outer device name in refold."
                                                               Just z  -> z
                                             --This is the interior device
                                             (ii,io) = case Map.lookup dev m of
                                                               Nothing -> error "procCL: Encountered an unknown reference on inner device name in refold."
                                                               Just z  -> z
                                             f1' = f1 {funDefnName="fout"}
                                             f2' = f2 {funDefnName="fin"}
                                 in "library ieee;\n"
                                  ++ "use ieee.std_logic_1164.all;\n"
                                  ++ "-- Uncomment the following line if VHDL primitives are in use.\n"
                                  ++ "use work.prims.all;\n"
                                  ++ "entity " ++ n ++ " is\n"
                                  ++ "  Port ( clk : in std_logic ;\n"
                                  ++ "         input : in std_logic_vector (0 to " ++ show (i-1) ++ ");\n"
                                  ++ "         output : out std_logic_vector (0 to " ++ show (o-1) ++ "));\n"
                                  ++ "end " ++ n ++ ";\n"
                                  ++ "architecture behavioral of " ++ n ++ " is\n"
                                  ++ indent ("signal dinput  : std_logic_vector(0 to " ++ show (ii-1) ++ ");\n")
                                  ++ indent ("signal doutput : std_logic_vector(0 to " ++ show (io-1) ++ ");\n")
                                  ++ indent (concatMap vFunDefnProto [f1',f2']) ++ "\n"
                                  ++ indent (concatMap vFunDefn [f1',f2']) ++ "\n"
                                  ++ "begin\n"
                                  ++ indent "dinput  <= fin(doutput,input);\n"
                                  ++ indent ("dev : entity work." ++ dev ++ "(behavioral)\n")
                                  ++ (indent . indent) ("port map (clk,dinput,doutput);\n\n")
                                  ++ indent "output <= fout(doutput);\n"
                                  ++ "\nend behavioral;\n"

procCL _ a  = error $ "procCL: Unhandled tree structure encountered: " ++ (show a)

pars :: Int -> Int -> String -> [(String,(Int,Int))] -> String
pars i o n devs = "library ieee;\n"
        ++ "use ieee.std_logic_1164.all;\n"
        ++ "-- Uncomment the following line if VHDL primitives are in use.\n"
        ++ "use work.prims.all;\n"
        ++ "entity " ++ n ++ " is\n"
        ++ "  Port ( clk : in std_logic ;\n"
        ++ "         input : in std_logic_vector (0 to " ++ show (i-1) ++ ");\n"
        ++ "         output : out std_logic_vector (0 to " ++ show (o-1) ++ "));\n"
        ++ "end " ++ n ++ ";\n"
        ++ "architecture behavioral of " ++ n ++ " is\n"
        ++ indent (sigdecls devs)
        ++ "begin\n"
        ++ indent (siginlinks devs)
        ++ indent (portMaps devs)
        ++ indent (sigoutlinks devs)
        ++ "\nend behavioral;\n"


main :: Int -> Int -> String -> String
main i o n = "library ieee;\n"
        ++ "use ieee.std_logic_1164.all;\n"
        ++ "-- Uncomment the following line if VHDL primitives are in use.\n"
        ++ "use work.prims.all;\n"
        ++ "entity main is\n"
        ++ "  Port ( clk : in std_logic ;\n"
        ++ "         input : in std_logic_vector (0 to " ++ show (i-1) ++ ");\n"
        ++ "         output : out std_logic_vector (0 to " ++ show (o-1) ++ "));\n"
        ++ "end main;\n"
        ++ "architecture structural of main is\n"
        ++ "begin\n"
        ++ indent ("dev : entity work." ++ n ++ "(behavioral)\n")
        ++ (indent . indent) ("port map (clk,input,output);\n\n")
        ++ "\nend structural;\n"


sigdecls  :: [(String,(Int,Int))] -> String
sigdecls [] = ""
sigdecls ((n,(iw,ow)):xs) =    "signal " ++ n ++ "input  : std_logic_vector(0 to " ++ show (iw - 1) ++ ");\n"
                                    ++ "signal " ++ n ++ "output : std_logic_vector(0 to " ++ show (ow - 1) ++ ");\n"
                                    ++ sigdecls xs

siginlinks :: [(String,(Int,Int))] -> String
siginlinks xs = siginlinks' 0 xs
  where
    siginlinks' _ [] = ""
    siginlinks' i ((n,(iw,_)):xs) = n ++ "input <= input(" ++ show i ++ " TO " ++ show (i + iw - 1) ++ ");\n"
                                      ++ siginlinks' (i+iw) xs
    siginlinks _ _ = error "siginlinks: encountered a bad case"

sigoutlinks :: [(String,(Int,Int))] -> String
sigoutlinks xs = "output <= " ++ sigoutlinks' 0 xs
  where
    sigoutlinks' _ [] = error "sigoutlinks: shouldn't have encountered an empty list of entities" 
    sigoutlinks' o ((n,(_,ow)):[]) = n ++ "output;"
    sigoutlinks' o ((n,(_,ow)):xs) = n ++ "output & " ++ sigoutlinks' (o+ow) xs  

portMaps :: [(String,(Int,Int))] -> String
portMaps [] = ""
portMaps ((n,_):xs) = n ++ "dev : entity work." ++ n ++ "(behavioral)\n"
                        ++ "  port map (clk," ++ n ++ "input," ++ n ++ "output);\n\n" ++ (portMaps xs)

toVHDL :: String -> Prog -> String
toVHDL e p = "library ieee;\n"
        ++ "use ieee.std_logic_1164.all;\n"
        ++ "-- Uncomment the following line if VHDL primitives are in use.\n"
        ++ "use work.prims.all;\n"
        ++ "entity " ++ e ++ " is\n"
        ++ "  Port ( clk : in std_logic ;\n"
        ++ "         input : in std_logic_vector (0 to " ++ show (inputSize (progHeader p)-1) ++ ");\n"
        ++ "         output : out std_logic_vector (0 to " ++ show (outputSize (progHeader p)-1) ++ "));\n"
        ++ "end " ++ e ++ ";\n"
        ++ "\n"
        ++ "architecture behavioral of rewire is\n"
        ++ indent ("type control_state is (" ++ intercalate "," (stateNames (progHeader p)) ++ ");\n")
        ++ indent (concatMap (++"\n") vFunProtos)
        ++ indent (concatMap (++"\n") vFunDefns)
        ++ indent (curControlFlopDecl ++ "\n")
        ++ indent (nextControlFlopDecl ++ "\n")
        ++ indent (inputFlopDecl ++ "\n")
        ++ indent (concatMap (++"\n") curFlopDecls)
        ++ indent (concatMap (++"\n") nextFlopDecls)
        ++ "begin\n"
        ++ indent loopProcess ++ "\n"
        ++ indent flopProcess ++ "\n"
        ++ "end behavioral;\n"
  where varNames = map regDeclName (regDecls (progHeader p))

        vFunProtos = map vFunDefnProto (funDefns (progHeader p))
        vFunDefns  = map vFunDefn (funDefns (progHeader p))

        curControlFlopDecl = "signal " ++ flopName "control" ++ " : control_state := " ++ startState (progHeader p) ++ ";"
        nextControlFlopDecl = "signal " ++ flopNextName "control" ++ " : control_state := " ++ startState (progHeader p) ++ ";"

        inputFlopDecl = "signal " ++ flopName "input" ++ " : " ++ vTy (TyBits (inputSize (progHeader p))) ++ " := (others => '0');"

        curFlopDecls  = map curFlopDecl (regDecls (progHeader p))
        curFlopDecl d = "signal " ++ flopName (regDeclName d) ++ " : " ++ vTy (regDefnTy d) ++ " := " ++ vInit (regDefnTy d) ++ ";"
        
        nextFlopDecls  = map nextFlopDecl (regDecls (progHeader p))
        nextFlopDecl d = "signal " ++ flopNextName (regDeclName d) ++ " : " ++ vTy (regDefnTy d) ++ " := " ++ vInit (regDefnTy d) ++ ";"

        loopProcess = "-- Logic loop process.\n"
                   ++ "process (" ++ intercalate "," loopSensitivityList ++ ")\n"
                   ++ indent (loopControlTmpDecl ++ "\n")
                   ++ indent (loopInputTmpDecl ++ "\n")
                   ++ indent (concatMap (++"\n") loopTmpDecls)
                   ++ indent (loopOutputTmpDecl ++ "\n")
                   ++ "begin\n"
                   ++ indent "-- Read reg temps.\n"
                   ++ indent (loopControlTmpInit ++ "\n")
                   ++ indent (loopInputTmpInit ++ "\n")
                   ++ indent (concatMap (++"\n") loopTmpInits)
                   ++ indent "output_tmp := (others => '0');\n"
                   ++ indent "-- Loop body.\n"
                   ++ indent (loopBody ++ "\n")
                   ++ indent "-- Write back reg temps.\n"
                   ++ indent (loopControlTmpWriteback ++ "\n")
                   ++ indent (concatMap (++"\n") loopTmpWritebacks)
                   ++ indent "-- Update output line.\n"
                   ++ indent "output <= output_tmp;\n"
                   ++ "end process;\n"
        loopSensitivityList     = [flopName "control",flopName "input"] ++ map flopName varNames
        loopControlTmpDecl      = "variable control : control_state;"
        loopInputTmpDecl        = "variable input_tmp : " ++ vTy (TyBits (inputSize (progHeader p))) ++ ";"
        loopTmpDecls            = map loopTmpDecl (regDecls (progHeader p))
        loopTmpDecl d           = "variable " ++ regDeclName d ++ " : " ++ vTy (regDefnTy d) ++ " := " ++ vInit (regDefnTy d) ++ ";"
        loopOutputTmpDecl       = "variable output_tmp : " ++ vTy (TyBits (outputSize (progHeader p))) ++ ";"
        loopControlTmpInit      = "control := " ++ flopName "control" ++ ";"
        loopInputTmpInit        = "input_tmp := " ++ flopName "input" ++ ";"
        loopTmpInits            = map loopTmpInit varNames
        loopTmpInit n           = n ++ " := " ++ flopName n ++ ";"
        loopBody                = vCmd (progBody p)
        loopControlTmpWriteback = flopNextName "control" ++ " <= control;"
        loopTmpWritebacks       = map loopTmpWriteback varNames
        loopTmpWriteback n      = flopNextName n ++ " <= " ++ n ++ ";"

        flopProcess = "-- Flip flop update process.\n"
                   ++ "process (" ++ intercalate "," flopSensitivityList ++ ")\n"
                   ++ "begin\n"
                   ++ indent ("if clk'event and clk='1' then\n"
                           ++ indent (inputFlopUpdate ++ "\n")
                           ++ indent (controlFlopUpdate ++ "\n")
                           ++ indent (concatMap (++"\n") varFlopUpdates)
                           ++ "end if;\n")
                   ++ "end process;\n"
        flopSensitivityList = ["clk","input"] ++ map flopNextName varNames
        inputFlopUpdate     = flopName "input" ++ " <= input;"
        flopUpdate n        = flopName n ++ " <= " ++ flopNextName n ++ ";"
        controlFlopUpdate   = flopUpdate "control"
        varFlopUpdates      = map flopUpdate varNames
{-
        ++ "architecture behavioral of " ++ e ++ " is\n"
        ++ "  type control_state is (" ++ intercalate "," (stateNames (progHeader p)) ++ ");\n"
        ++ indent (concatMap vFunDefnProto (funDefns (progHeader p))) ++ "\n"
        ++ indent (concatMap vFunDefn (funDefns (progHeader p))) ++ "\n"
        ++ "begin\n"
        ++ indent (
           "process (clk)\n"
        ++ indent (concatMap ((++"\n") . vRegDecl) (regDecls (progHeader p)))
        ++ "  variable state : control_state := " ++ startState (progHeader p)  ++ ";\n"
        ++ "  variable nextout : std_logic_vector (0 to " ++ show (outputSize (progHeader p)-1) ++ ");\n"
        ++ "begin\n"
        ++ "  if clk'event and clk='1' then\n"
        ++ "    output <= nextout;\n"
        ++ indent (indent (vCmd (progBody p))) ++ "\n"
        ++ "  end if;\n"
        ++ "end process;\n"
           )
        ++ "end behavioral;\n"
--        vHeader (progHeader p) ++ vCmd (progBody p)
-}
