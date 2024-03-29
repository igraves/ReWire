module DLX where

import Prelude hiding (and,or,seq,(||))
import Boilerplate
import Control.Monad.Resumption.Reactive
import Control.Monad.State hiding (when)
import Control.Monad.Identity hiding (when)

{-
loop = do inp <- getInputs
          case rstIn inp of
            One  -> reset
            Zero -> do ie   <- getIEFlag
                       case (ie,intIn inp) of
                         (One,One) -> interrupt
                         _         -> case dataIn inp of
                           W8 Zero Zero Zero Zero  rEn  wEn   b0   b1 -> mem rEn wEn (mkReg b0 b1)
-}

instrdec w32 = 
   let
     t6 = top6 w32
   in
   case t6 of 
        W6 Zero Zero Zero Zero Zero Zero -> decodeR opcode rs1' rs2' rd'
            where (rs1,rs2,rd,opcode) = rtype w32
                  rs1'                = decodeReg rs1
                  rs2'                = decodeReg rs2
                  rd'                 = decodeReg rd
        W6 Zero Zero Zero Zero One Zero  -> decodeJ offset t6
            where (opcode,offset) = jtype w32 
        W6 Zero Zero Zero Zero One One   -> decodeJ offset t6
            where (opcode,offset) = jtype w32 
        _                                -> decodeI opcode rs1' rd' imm
            where (opcode,rs1,rd,imm) = itype w32
                  rs1'                = decodeReg rs1
                  rd'                 = decodeReg rd
              
reset = putOutputs initOutputs

decodeR :: W6       -> 
           Register -> 
           Register -> 
           Register -> 
           ReacT Inputs Outputs (StateT DLXState Identity) ()
decodeR opcode rs1 rs2 rd = case opcode of
  W6 One Zero Zero Zero Zero Zero -> add rd rs1 rs2 -- "ADD  00100000 x20"
  W6 One Zero Zero One Zero Zero  -> and rd rs1 rs2 -- "AND  00100100 x24"
  W6 One Zero Zero One Zero One   -> or rd rs1 rs2  -- "OR   0x25 100101"
  W6 One Zero One Zero Zero Zero  -> seq rd rs1 rs2 -- "SEQ  0x28 101000"
  W6 One Zero One One Zero Zero   -> sle rd rs1 rs2 -- "SLE  0x2c 101100"
  W6 Zero Zero Zero One Zero Zero -> error "SLL  0x04 000100"
  W6 One Zero One Zero One Zero   -> slt rd rs1 rs2 -- "SLT  0x2a 101010"
  W6 One Zero One Zero Zero One   -> sne rd rs1 rs2 -- "SNE  0x29 101001"
  W6 Zero Zero Zero One One One   -> error "SRA  0x07 000111"
  W6 Zero Zero Zero One One Zero  -> error "SRL  0x06 000110"
  W6 One Zero Zero Zero One Zero  -> error "SUB  0x22 100010"
  W6 One Zero Zero One One Zero   -> error "XOR  0x26 100110"


decodeJ offset w6 = case w6 of
  W6 Zero Zero Zero Zero One Zero -> j offset   -- "J    00000010 x02"
  W6 Zero Zero Zero Zero One One  -> jal offset -- "JAL  00000011 x03"

decodeI :: W6       -> 
           Register -> 
           Register -> 
           W16      -> 
           ReacT Inputs Outputs (StateT DLXState Identity) ()
decodeI opcode rs1 rd imm = case opcode of
  W6 Zero Zero One Zero Zero Zero -> addi rd rs1 imm -- "ADDI 00001000 x08"
  W6 Zero Zero One One Zero Zero  -> andi rd rs1 imm -- "ANDI 00001100 x0c"
  W6 Zero Zero Zero One Zero Zero -> beqz rs1 imm    -- "BEQZ 00000100 x04"
  W6 Zero Zero Zero One Zero One  -> bnez rs1 imm    -- "BNEZ 00000101 x05"
  W6 Zero One Zero Zero One One   -> jalr rs1        -- "JALR 00010011 x13"
  W6 Zero One Zero Zero One Zero  -> jr rs1          -- "JR   00010010 x12"
  W6 Zero Zero One One One One    -> lhi rs1 imm     -- "LHI  0x0f 001111"
  W6 One Zero Zero Zero One One   -> lw rs1 rd imm   -- "LW   0x23 100011"
  W6 Zero Zero One One Zero One   -> ori rd rs1 imm  -- "ORI  0x0d 001101"
  W6 Zero One One Zero Zero Zero  -> seqi rd rs1 imm -- "SEQI 0x18 011000"
  W6 Zero One One One Zero Zero   -> slei rd rs1 imm -- "SLEI 0x1c 011100"
  W6 Zero One Zero One Zero Zero  -> error "SLLI 0x14 010100"
  W6 Zero One One Zero One Zero   -> slti rd rs1 imm -- "SLTI 0x1a 011010"
  W6 Zero One One Zero Zero One   -> snei rd rs1 imm -- "SNEI 0x19 011001"
  W6 Zero One Zero One One One    -> error "SRAI 0x17 010111"
  W6 Zero One Zero One One Zero   -> error "SRLI 0x16 010110"
  W6 Zero Zero One Zero One Zero  -> error "SUBI 0x0a 001010"
  W6 One Zero One Zero One One    -> error "SW   0x2b 101011"
  W6 Zero Zero One One One Zero   -> error "XORI 0x0e 001110"
  _                               -> error "unknown opcode"


--
-- Instructions
--

add :: Register -> 
       Register -> 
       Register ->
       ReacT Inputs Outputs (StateT DLXState Identity) ()
add rd rs1 rs2 = do v1             <- getReg rs1
                    v2             <- getReg rs2
                    let (cout,v) =  plusCW32 v1 v2 Zero
                    putReg rd v
                    tick

addi :: Register -> 
        Register -> 
        W16      -> 
        ReacT Inputs Outputs (StateT DLXState Identity) ()
addi rD rS imm = do vS <- getReg rS
                    let signext_imm = signextend16_32 imm
                    let sum         = plusW32 vS signext_imm Zero
                    putReg rD sum
                    tick

and :: Register -> 
       Register -> 
       Register -> 
       ReacT Inputs Outputs (StateT DLXState Identity) ()
and rd rs1 rs2 = do v1 <- getReg rs1
                    v2 <- getReg rs2
                    putReg rd (andW32 v1 v2)

andi :: Register -> 
        Register -> 
        W16      -> 
        ReacT Inputs Outputs (StateT DLXState Identity) ()
andi rd rs1 imm = do v1 <- getReg rs1
                     let imm32 = zero16 || imm 
                     putReg rd (andW32 v1 imm32)

beqz :: Register -> W16 -> DLXM ()
beqz rs1 offset = do v1 <- getReg rs1
                     let se_offset = signextend16_32 offset 
                     pc <- getPC
                     let pc'       = plusW32 (plusW32 pc w32_4 Zero) se_offset Zero
                     if zero v1 then putPC pc' >> tick else tick

bnez :: Register -> W16 -> DLXM ()
bnez rs1 offset = do v1 <- getReg rs1
                     let se_offset = signextend16_32 offset 
                     pc <- getPC
                     let pc'       = plusW32 (plusW32 pc w32_4 Zero) se_offset Zero
                     if zero v1 then tick else putPC pc' >> tick 

zero w32 = case w32 of { (W32 Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero) -> True ; _ -> False }

-- Jump
j :: W26 -> DLXM ()
j offset = do pc <- getPC
              let signext_offset = signextend26_to_32 offset
              let pc' = plusW32 pc (plusW32 signext_offset w32_4 Zero) Zero
              putPC pc'
              tick

-- Jump and Link
jal :: W26 -> DLXM ()
jal offset = do pc <- getPC
                let signext_offset = signextend26_to_32 offset
                let pc'  = plusW32 pc (plusW32 signext_offset w32_4 Zero) Zero
                let r31' = plusW32 pc w32_8 Zero
                putReg R31 r31'
                putPC pc'
                tick

-- Jump and Link register
jalr :: Register -> DLXM ()
jalr rs1 = do pc <- getPC
              let r31' = plusW32 pc w32_8 Zero
              putReg R31 r31'
              dst <- getReg rs1
              putPC dst
              tick

-- Jump register
jr :: Register -> DLXM ()
jr rs1 = do pc <- getPC
            dst <- getReg rs1
            putPC dst
            tick

-- Load high bits immediate
lhi :: Register -> W16 -> DLXM ()
lhi rd imm = let 
               w32 = imm || zero16
             in 
               putReg rd w32

-- Load Word
lw :: Register -> 
      Register -> 
      W16      -> 
      ReacT Inputs Outputs (StateT DLXState Identity) ()
lw rs1 rd offset = do base <- getReg rs1
                      eff_addr <- return $ plusW32 base (signextend16_32 offset) Zero
                      putWeOut Zero
                      putAddrOut eff_addr
                      tick
                      v <- getDataIn
                      putReg rd v

-- It seems weird to me that there is no checking of whether 
-- there is a special register involved with the next two.
-- Where does that happen?

-- Move general purpose to special
movi2s :: Register -> 
          Register -> 
          ReacT Inputs Outputs (StateT DLXState Identity) ()
movi2s rd rs1    = getReg rs1 >>= putReg rd >> tick

-- Move special to general purpose
movs2i :: Register -> 
          Register -> 
          ReacT Inputs Outputs (StateT DLXState Identity) ()
movs2i rd rs1    = getReg rs1 >>= putReg rd >> tick

-- No Op
nop :: DLXM ()
nop              = return ()

or :: Register -> 
      Register -> 
      Register -> 
      ReacT Inputs Outputs (StateT DLXState Identity) ()
or rd rs1 rs2 = do v1      <- getReg rs1
                   v2      <- getReg rs2
                   let vd =  orW32 v1 v2
                   putReg rd vd
                   tick

ori :: Register -> 
       Register -> 
       W16      -> 
       ReacT Inputs Outputs (StateT DLXState Identity) ()
ori rd rs1 imm = do v1 <- getReg rs1
                    let imm32 = zero16 || imm 
                    putReg rd (orW32 v1 imm32)

-- Set if equal
seq :: Register -> 
       Register -> 
       Register ->
       ReacT Inputs Outputs (StateT DLXState Identity) ()
seq rd rs1 rs2 = do v1 <- getReg rs1
                    v2 <- getReg rs2
                    if v1==v2
                      then
                        putReg rd one32
                      else
                        putReg rd zero32

-- Set if equal to immediate
seqi :: Register -> 
        Register -> 
        W16      ->
        ReacT Inputs Outputs (StateT DLXState Identity) ()
seqi rd rs1 imm = do v1 <- getReg rs1
                     if v1 == signextend16_32 imm
                       then
                         putReg rd one32
                       else
                         putReg rd zero32

-- Set if less than or equal
sle :: Register -> 
       Register -> 
       Register ->
       ReacT Inputs Outputs (StateT DLXState Identity) ()
sle rd rs1 rs2 = do v1 <- getReg rs1
                    v2 <- getReg rs2
                    if v1 `w32_lte` v2
                      then
                        putReg rd one32
                      else
                        putReg rd zero32

-- Set if less than or equal to immediate
slei :: Register -> 
        Register -> 
        W16      ->
        ReacT Inputs Outputs (StateT DLXState Identity) ()
slei rd rs1 imm = do v1 <- getReg rs1
                     if v1 `w32_lte` signextend16_32 imm
                       then
                         putReg rd one32
                       else
                         putReg rd zero32

-- Set if less than
slt :: Register -> 
       Register -> 
       Register ->
       ReacT Inputs Outputs (StateT DLXState Identity) ()
slt rd rs1 rs2 = do v1 <- getReg rs1
                    v2 <- getReg rs2
                    if v1 `w32_lt` v2
                      then
                        putReg rd one32
                      else
                        putReg rd zero32

-- Set if less than immediate
slti :: Register -> 
        Register -> 
        W16      ->
        ReacT Inputs Outputs (StateT DLXState Identity) ()
slti rd rs1 imm = do v1 <- getReg rs1
                     if v1 `w32_lt` signextend16_32 imm
                       then
                         putReg rd one32
                       else
                         putReg rd zero32

-- Set if not equal
sne :: Register -> 
       Register -> 
       Register ->
       ReacT Inputs Outputs (StateT DLXState Identity) ()
sne rd rs1 rs2 = do v1 <- getReg rs1
                    v2 <- getReg rs2
                    if v1 `w32_ne` v2
                      then
                        putReg rd one32
                      else
                        putReg rd zero32

-- Set if not equal to immediate
snei :: Register -> 
        Register -> 
        W16      ->
        ReacT Inputs Outputs (StateT DLXState Identity) ()
snei rd rs1 imm = do v1 <- getReg rs1
                     if v1 `w32_ne` signextend16_32 imm
                       then
                         putReg rd one32
                       else
                         putReg rd zero32


data Register = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7
              | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
              | R16 | R17 | R18 | R19 | R20 | R21 | R22 | R23
              | R24 | R25 | R26 | R27 | R28 | R29 | R30 | R31

data Inputs = Inputs { dataIn :: W32,
                       rstIn  :: Bit,
                       intIn  :: Bit 
                     }

data Outputs = Outputs { addrOut :: W32,
                         dataOut :: W32,
                     --, not sure what this stuff is.
                         weOut   :: Bit,
                         iackOut :: Bit 
                       }


{- http://www.csee.umbc.edu/courses/undergraduate/411/spring96/dlx.html -}

--
-- Breaks a W32 into opcode and value fields of a DLX J-type instruction.
--
jtype :: W32 -> (W6, W26)
jtype (W32 b31 b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16 
           b15 b14 b13 b12 b11 b10  b9  b8  b7  b6  b5  b4  b3  b2  b1  b0) 
        = (opcode,value)
          where opcode = W6 b31 b30 b29 b28 b27 b26
                value  = W26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16 
                             b15 b14 b13 b12 b11 b10  b9  b8  b7  b6
                              b5  b4  b3  b2  b1  b0

--
-- Breaks a W32 into opcode, source and destination register, and immediate
-- fields of a DLX I-type instruction.
--
itype :: W32 -> (W6, W5, W5, W16)
itype (W32 b31 b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16 
           b15 b14 b13 b12 b11 b10  b9  b8  b7  b6  b5  b4  b3  b2  b1  b0) 
        = (opcode,rs1,rd,immediate)
          where opcode    = W6 b31 b30 b29 b28 b27 b26
                rs1       = W5 b25 b24 b23 b22 b21
                rd        = W5 b20 b19 b18 b17 b16 
                immediate = W16 b15 b14 b13 b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0

rtype :: W32 -> (W5, W5, W5, W6)
rtype (W32 b31 b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16 
           b15 b14 b13 b12 b11 b10  b9  b8  b7  b6  b5  b4  b3  b2  b1  b0) 
        = (rs1,rs2,rd,opcode)
          where rs1       = W5 b25 b24 b23 b22 b21
                rs2       = W5 b20 b19 b18 b17 b16 
                rd        = W5 b15 b14 b13 b12 b11
                opcode    = W6 b5 b4 b3 b2 b1 b0

data DLXState = DLXState { inputs  :: Inputs,
                           outputs :: Outputs,
                           zFlag   :: Bit,    -- some of this stuff has been
                           ieFlag  :: Bit,    -- snarfed willy nilly from MiniIsa.hs.

                           overFlw :: Bit,

                           pc      :: W32,                           
                           iar     :: W32, -- a "special register" along with FPSR
                           r0      :: W32,
                           r1      :: W32,
                           r2      :: W32,
                           r3      :: W32,
                           r4      :: W32,
                           r5      :: W32,
                           r6      :: W32,
                           r7      :: W32,
                           r8      :: W32,
                           r9      :: W32,
                           r10     :: W32,
                           r11     :: W32,
                           r12     :: W32,
                           r13     :: W32,
                           r14     :: W32,
                           r15     :: W32,
                           r16     :: W32,
                           r17     :: W32,
                           r18     :: W32,
                           r19     :: W32,
                           r20     :: W32,
                           r21     :: W32,
                           r22     :: W32,
                           r23     :: W32,
                           r24     :: W32,
                           r25     :: W32,
                           r26     :: W32,
                           r27     :: W32,
                           r28     :: W32,
                           r29     :: W32,
                           r30     :: W32,
                           r31     :: W32
                         }

type DLXM = ReacT Inputs Outputs (StateT DLXState Identity)

getState :: DLXM DLXState
getState = lift get

putState :: DLXState -> DLXM ()
putState = lift . put

putWeOut :: Bit -> DLXM ()
putWeOut b = do o <- getOutputs
                putOutputs (o { weOut = b })

initInputs  = Inputs  { dataIn  = w32_0, rstIn = Zero, intIn = Zero }
initOutputs = Outputs { addrOut = w32_0, dataOut = w32_0, weOut = Zero, iackOut = Zero }

getDataIn :: DLXM W32
getDataIn = do i <- getInputs
               return (dataIn i)


getReg :: Register -> DLXM W32
getReg r = do s <- getState
              case r of
                R0  -> return (r0 s)
                R1  -> return (r1 s)
                R2  -> return (r2 s)
                R3  -> return (r3 s)
                R4  -> return (r4 s)
                R5  -> return (r5 s)
                R6  -> return (r6 s)
                R7  -> return (r7 s)
                R8  -> return (r8 s)
                R9  -> return (r9 s)
                R10 -> return (r10 s)
                R11 -> return (r11 s)
                R12 -> return (r12 s)
                R13 -> return (r13 s)
                R14 -> return (r14 s)
                R15 -> return (r15 s)
                R16 -> return (r16 s)
                R17 -> return (r17 s)
                R18 -> return (r18 s)
                R19 -> return (r19 s)
                R20 -> return (r20 s)
                R21 -> return (r21 s)
                R22 -> return (r22 s)
                R23 -> return (r23 s)
                R24 -> return (r24 s)
                R25 -> return (r25 s)
                R26 -> return (r26 s)
                R27 -> return (r27 s)
                R28 -> return (r28 s)
                R29 -> return (r29 s)
                R30 -> return (r30 s)
                R31 -> return (r31 s)

putReg :: Register -> W32 -> DLXM ()
putReg r v = do s <- getState
                case r of
                  R0  -> putState (s { r0 = v })
                  R1  -> putState (s { r1 = v })
                  R2  -> putState (s { r2 = v })
                  R3  -> putState (s { r3 = v })
                  R4  -> putState (s { r4 = v })
                  R5  -> putState (s { r5 = v })
                  R6  -> putState (s { r6 = v })
                  R7  -> putState (s { r7 = v })
                  R8  -> putState (s { r8 = v })
                  R9  -> putState (s { r9 = v })
                  R10 -> putState (s { r10 = v })
                  R11 -> putState (s { r11 = v })
                  R12 -> putState (s { r12 = v })
                  R13 -> putState (s { r13 = v })
                  R14 -> putState (s { r14 = v })
                  R15 -> putState (s { r15 = v })
                  R16 -> putState (s { r16 = v })
                  R17 -> putState (s { r17 = v })
                  R18 -> putState (s { r18 = v })
                  R19 -> putState (s { r19 = v })
                  R20 -> putState (s { r20 = v })
                  R21 -> putState (s { r21 = v })
                  R22 -> putState (s { r22 = v })
                  R23 -> putState (s { r23 = v })
                  R24 -> putState (s { r24 = v })
                  R25 -> putState (s { r25 = v })
                  R26 -> putState (s { r26 = v })
                  R27 -> putState (s { r27 = v })
                  R28 -> putState (s { r28 = v })
                  R29 -> putState (s { r29 = v })
                  R30 -> putState (s { r30 = v })
                  R31 -> putState (s { r31 = v })

getPC :: DLXM W32
getPC = do s <- getState
           return (pc s)

putPC :: W32 -> DLXM ()
putPC v = do s <- getState
             putState (s { pc = v })

getIAR :: DLXM W32
getIAR = do s <- getState
            return (iar s)

putIAR :: W32 -> DLXM ()
putIAR v = do s <- getState
              putState (s { iar = v })

getInputs :: DLXM Inputs
getInputs = do s <- getState
               return (inputs s)

putInputs :: Inputs -> DLXM ()
putInputs i = do s <- getState
                 putState (s { inputs = i })

getOutputs :: DLXM Outputs 
getOutputs   = do s <- getState
                  return (outputs s)

putOutputs :: Outputs -> DLXM ()
putOutputs o = do s <- getState
                  putState (s { outputs = o })

putAddrOut :: W32 -> DLXM ()
putAddrOut a = do o <- getOutputs
                  putOutputs (o { addrOut = a })

tick :: DLXM ()
tick = do o <- getOutputs
          i <- signal o
          putInputs i

signextend26_to_32 :: W26 -> W32
signextend26_to_32 (W26 Zero b24 b23 b22 b21 b20 b19 b18 b17 b16 b15 b14 
                        b13 b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0)
                         = W32 Zero Zero Zero Zero Zero Zero Zero 
                               b24 b23 b22 b21 b20 b19 b18 b17 
                               b16 b15 b14 b13 b12 b11 b10 b9 
                               b8 b7 b6 b5 b4 b3 b2 b1 b0
signextend26_to_32 (W26 One b24 b23 b22 b21 b20 b19 b18 b17 b16 b15 b14 
                        b13 b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0)
                         = W32 One One One One One One One 
                               b24 b23 b22 b21 b20 b19 b18 b17 
                               b16 b15 b14 b13 b12 b11 b10 b9 
                               b8 b7 b6 b5 b4 b3 b2 b1 b0



--
-- I'm not sure this is exactly the right thing to do.
--
signextend16_32 :: W16 -> W32
signextend16_32 (W16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15)
    = W32 Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero Zero 
            b0   b1   b2   b3   b4   b5   b6   b7   b8   b9  b10  b11  b12  b13  b14  b15
decodeReg w5 = case w5 of
  W5 Zero Zero Zero Zero Zero -> R0
  W5 Zero Zero Zero Zero One  -> R1
  W5 Zero Zero Zero One Zero  -> R2
  W5 Zero Zero Zero One One   -> R3
  W5 Zero Zero One Zero Zero  -> R4
  W5 Zero Zero One Zero One   -> R5
  W5 Zero Zero One One Zero   -> R6
  W5 Zero Zero One One One    -> R7
  W5 Zero One Zero Zero Zero  -> R8
  W5 Zero One Zero Zero One   -> R9
  W5 Zero One Zero One Zero   -> R10
  W5 Zero One Zero One One    -> R11
  W5 Zero One One Zero Zero   -> R12
  W5 Zero One One Zero One    -> R13
  W5 Zero One One One Zero    -> R14
  W5 Zero One One One One     -> R15
----
  W5 One Zero Zero Zero Zero -> R16
  W5 One Zero Zero Zero One  -> R17
  W5 One Zero Zero One Zero  -> R18
  W5 One Zero Zero One One   -> R19
  W5 One Zero One Zero Zero  -> R20
  W5 One Zero One Zero One   -> R21
  W5 One Zero One One Zero   -> R22
  W5 One Zero One One One    -> R23
  W5 One One Zero Zero Zero  -> R24
  W5 One One Zero Zero One   -> R25
  W5 One One Zero One Zero   -> R26
  W5 One One Zero One One    -> R27
  W5 One One One Zero Zero   -> R28
  W5 One One One Zero One    -> R29
  W5 One One One One Zero    -> R30
  W5 One One One One One     -> R31
