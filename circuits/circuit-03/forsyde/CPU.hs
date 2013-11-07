{-# LANGUAGE TemplateHaskell #-}
module CPU where

import ForSyDe
import ForSyDe.Bit
import Prelude hiding (not)

import Data.Param.FSVec hiding ((++))
import qualified Data.Param.FSVec as V
import Data.TypeLevel.Num hiding ((==), (+))
import qualified Data.TypeLevel.Num as N

import Data.Int (Int16)

import ALUSyn
import Register hiding (WordType)


-- | The type of memory addresses
type AddrType = Int16

-- | The program counter is a straightforward counter, which can be reset and set to a particular
-- value, and counts from this value upwards
programCounter :: Signal Bit       -- ^ reset
               -> Signal Bit       -- ^ set
               -> Signal AddrType  -- ^ value to be set
               -> Signal AddrType  -- ^ value currently in the counter
programCounter = scanld3SY "programCounter" nextStateFun 0
    where
        nextStateFun =
            $(newProcFun [d| f :: AddrType -> Bit -> Bit -> AddrType -> AddrType
                             f cur reset set new = if reset == H then 0
                                                   else if set == H then new
                                                   else cur + 1 |])

-- Expected behaviour: counts normally from zero, then is set to one and counts from there.
-- Finally, while in a high count, is reset and starts over from zero.
testPC3 :: Bool
testPC3 = (simulate pcSys) resets sets vals == expected
    where
        pcSys    = newSysDef programCounter "pcSys" ["reset", "set", "addr"] ["out"]
        (r, s)   = (H, H)  -- nicknames for reset and set
        x        = 0  -- nickname for "don't care"
        expected = [0, 1, 2, 3, 1, 2, 3, 0, 1, 2, 3, 4]
        (resets, sets, vals) = unzip3 inputs
        inputs = [ (L, L, x), (L, L, x), (L, L, x), (L, s, 1), (L, L, x), (L, L, x)
                 , (r, L, x), (L, L, x), (L, L, x), (L, L, x), (L, L, x), (L, L, x) ]


-- | Circuit to decide whether to set or not the program counter (PC), given the
-- Jump Condition bits from the instruction and the ZR and NG flags from the ALU
decideSetPC :: Signal (Bit, Bit, Bit) -> Signal Bit -> Signal Bit -> Signal Bit
decideSetPC = zipWith3SY "decideSetPC" decideFun
    where
        decideFun =
            $(newProcFun [d| f :: (Bit, Bit, Bit) -> Bit -> Bit -> Bit
                             f (jlt, jeq, jgt) stZR stNG = if stNG == H then jlt
                                                           else if stZR == H then jeq
                                                           else jgt |])

{-
testDecideSetPC :: Bool
testDecideSetPC = (simulate decideSys) conds stZRs stNGs == expected
    where
        decideSys = newSysDef decideSetPC "decideSys" ["cond", "stZR", "stNG"] ["setPC"]
        expected  = undefined
        (conds, stZRs, stNGs) = unzip3 inputSequence
        inputs = [ ((X,X,X),X,X), ((X,X,X),X,X), ((X,X,X),X,X), ((X,X,X),X,X)
                 , ((X,X,X),X,X), ((X,X,X),X,X), ((X,X,X),X,X), ((X,X,X),X,X)
                 , ((X,X,X),X,X), ((X,X,X),X,X), ((X,X,X),X,X), ((X,X,X),X,X)
                 , ((X,X,X),X,X), ((X,X,X),X,X), ((X,X,X),X,X), ((X,X,X),X,X)
                 , ((X,X,X),X,X), ((X,X,X),X,X), ((X,X,X),X,X), ((X,X,X),X,X)
                 , ((X,X,X),X,X), ((X,X,X),X,X), ((X,X,X),X,X), ((X,X,X),X,X)
                 , ((X,X,X),X,X), ((X,X,X),X,X), ((X,X,X),X,X), ((X,X,X),X,X)
                 , ((X,X,X),X,X), ((X,X,X),X,X), ((X,X,X),X,X), ((X,X,X),X,X) ]
-}


type HackInstruction = FSVec D16 Bit

type DestType = (Bit, Bit, Bit)
type JumpType = (Bit, Bit, Bit)

instructionDecoder :: Signal HackInstruction -> Signal (Bit, Bit, DestType, JumpType, ALUControl)
instructionDecoder = mapSY "instructionDecoder" decoderFun
    where
        decoderFun =
            $(newProcFun [d| f :: HackInstruction -> (Bit, Bit, DestType, JumpType, ALUControl)
                             f i = ( i ! d0
                                   , not (i ! d3)
                                   , (i ! d10,  i ! d11,  i ! d12)
                                   , (i ! d13,  i ! d14,  i ! d15)
                                   , (i ! d4,  i ! d5,  i ! d6,  i ! d7,  i ! d8,  i ! d9)
                                   ) |])


hackCPU :: Signal WordType  -- ^ inM: M value input (M = contents of RAM[A]
        -> HackInstruction  -- ^ instruction of Hack assembly
        -> Signal Bit       -- ^ reset
        -> ( Signal WordType  -- ^ outM: M value output
           , Signal Bit       -- ^ writeM: whether to write to M
           , Signal AddrType  -- ^ addressM: address of M in data memory
           , Signal AddrType  -- ^ pc: address of the next instruction
           )
hackCPU = undefined

{-

-- | A Hack instruction is a vector of 16 bits
type HackInstruction = (SB, SB, SB, SB, SB, SB, SB, SB, SB, SB, SB, SB, SB, SB, SB, SB)

cpu :: Int                  -- The word size of the CPU
    -> ( [Signal Bool]      -- inM: M value input (M = contents of RAM[A]
       , HackInstruction    -- instruction of Hack assembly
       , Signal Bool)       -- whether to restart current program or just continue
       -> ( [Signal Bool]   -- outM: M value output
         , Signal Bool      -- writeM: whether to write to M
         , [Signal Bool]    -- addressM: address of M in data memory
         , [Signal Bool] )  -- pc: address of the next instruction

cpu wordSize (inM, instruction, reset) = (outM, writeM, addressM, pc)
  where
    (i00,i01,i02,i03,i04,i05,i06,i07,i08,i09,i10,i11,i12,i13,i14,i15) = instruction
    insList = [i00,i01,i02,i03,i04,i05,i06,i07,i08,i09,i10,i11,i12,i13,i14,i15]

    outM                 = aluOut
    addressM             = aReg
    aReg                 = regN wordSize (aMux, setA)
    dReg                 = regN wordSize (aluOut, setD)
    aMux                 = mux (aFlag, (insList, aluOut))
    am                   = mux (cAM, (inM, aReg))
    (aluOut, stZR, stNG) = alu (dReg, am, cALU)
    pc                   = programCounter wordSize (reset, setPC, aReg)

    setPC = decideSetPC (cJump, stZR, stNG)
    setA  = or2 (inv aFlag, and2 (aFlag, writeA))
    setD  = and2 (aFlag, writeD)

    (aFlag, cAM, cDest, cJump, cALU) = instructionDecoder instruction
    (writeA, writeD, writeM)         = cDest

-}
