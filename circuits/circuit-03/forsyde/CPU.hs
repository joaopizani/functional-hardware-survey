{-# LANGUAGE TemplateHaskell #-}
module CPU where

import ForSyDe
import ForSyDe.Bit
import Prelude hiding (not)

import Data.Param.FSVec hiding ((++), unzip)
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
pc :: Signal Bit -> Signal Bit -> Signal AddrType -> Signal AddrType
pc = scanld3SY "programCounter" nextStateFun 0
    where nextStateFun = $(newProcFun [d| f :: AddrType -> Bit -> Bit -> AddrType -> AddrType
                                          f cur reset set new = if reset == H then 0
                                                                else if set == H then new
                                                                else cur + 1 |])

pcSysDef :: SysDef (Signal Bit -> Signal Bit -> Signal AddrType -> Signal AddrType)
pcSysDef = newSysDef pc "pcSys" ["reset", "set", "addr"] ["out"]

-- Expected behaviour: counts normally from zero, then is set to one and counts from there.
-- Finally, while in a high count, is reset and starts over from zero.
testPC3 :: Bool
testPC3 = (simulate pcSysDef) resets sets vals == expected
    where
        (r, s)   = (H, H)  -- nicknames for reset and set
        x        = 0  -- nickname for "don't care"
        expected = [0, 1, 2, 3, 1, 2, 3, 0, 1, 2, 3, 4]
        (resets, sets, vals) = unzip3 inputs
        inputs = [ (L, L, x), (L, L, x), (L, L, x), (L, s, 1), (L, L, x), (L, L, x)
                 , (r, L, x), (L, L, x), (L, L, x), (L, L, x), (L, L, x), (L, L, x) ]


type JumpType = (Bit, Bit, Bit)

-- | Circuit to decide whether to set or not the program counter (PC), given the
-- Jump Condition bits from the instruction and the ZR and NG flags from the ALU
decideJump :: Signal JumpType -> Signal ALUFlags -> Signal Bit
decideJump = zipWithSY "zipWithDecide" decideFun
    where decideFun = $(newProcFun [d| f :: (Bit, Bit, Bit) -> ALUFlags -> Bit
                                       f (jl,je,jg) (stZ,stN) = if stN == H then jl
                                                              else if stZ == H then je
                                                              else jg |])

decideJumpSysDef :: SysDef (Signal JumpType -> Signal ALUFlags -> Signal Bit)
decideJumpSysDef = newSysDef decideJump "decideSys" ["cond", "flags"] ["jump"]

testDecideJump :: Bool
testDecideJump = (simulate decideJumpSysDef) conds flags == expected
    where
        (conds, flags) = unzip inputs
        inputs = [ ((L,L,L),(L,L)), ((L,L,L),(L,H)), ((L,L,L),(H,L)), ((L,L,L),(H,H))
                 , ((L,L,H),(L,L)), ((L,L,H),(L,H)), ((L,L,H),(H,L)), ((L,L,H),(H,H))
                 , ((L,H,L),(L,L)), ((L,H,L),(L,H)), ((L,H,L),(H,L)), ((L,H,L),(H,H))
                 , ((L,H,H),(L,L)), ((L,H,H),(L,H)), ((L,H,H),(H,L)), ((L,H,H),(H,H))
                 , ((H,L,L),(L,L)), ((H,L,L),(L,H)), ((H,L,L),(H,L)), ((H,L,L),(H,H))
                 , ((H,L,H),(L,L)), ((H,L,H),(L,H)), ((H,L,H),(H,L)), ((H,L,H),(H,H))
                 , ((H,H,L),(L,L)), ((H,H,L),(L,H)), ((H,H,L),(H,L)), ((H,H,L),(H,H))
                 , ((H,H,H),(L,L)), ((H,H,H),(L,H)), ((H,H,H),(H,L)), ((H,H,H),(H,H)) ]
        expected = [ L, L, L, L
                   , H, L, L, L
                   , L, L, H, L
                   , H, L, H, L
                   , L, H, L, H
                   , H, H, L, H
                   , L, H, H, H
                   , H, H, H, H ]


type HackInstruction = FSVec D16 Bit

type DestType = (Bit, Bit, Bit)

instructionDecoder :: Signal HackInstruction -> Signal (Bit, Bit, DestType, JumpType, ALUControl)
instructionDecoder = mapSY "mapSYdecoder" decoderFun
    where
        decoderFun =
            $(newProcFun [d| f :: HackInstruction -> (Bit, Bit, DestType, JumpType, ALUControl)
                             f i = ( i ! d0
                                   , not (i ! d3)
                                   , (i ! d10,  i ! d11,  i ! d12)
                                   , (i ! d13,  i ! d14,  i ! d15)
                                   , (i ! d4,  i ! d5,  i ! d6,  i ! d7,  i ! d8,  i ! d9)
                                   ) |])

decoderSysDef :: SysDef (Signal HackInstruction -> Signal (Bit, Bit, DestType, JumpType, ALUControl))
decoderSysDef = newSysDef instructionDecoder "decoderSys" ["instruction"] ["ctrlBits"]


hackCPU :: Signal WordType  -- ^ inM: M value input (M = contents of RAM[A]
        -> HackInstruction  -- ^ instruction of Hack assembly
        -> Signal Bit       -- ^ reset
        -> Signal ( WordType  -- ^ outM: M value output
                 , Bit       -- ^ writeM: whether to write to M
                 , AddrType  -- ^ addressM: address of M in data memory
                 , AddrType  -- ^ pc: address of the next instruction
                 )
hackCPU inM instruction reset = zip4SY "zipOuts" aluOut writeM aReg nextInst
    where
        -- parts declaration
        mux2' l     = instantiate (l ++ ":mux") mux2SysDef
        aReg'       = instantiate "aReg" regSysDef
        dReg'       = instantiate "dReg" regSysDef
        alu'        = instantiate "alu" aluSysDef
        decideJump' = instantiate "decideJump" decideJumpSysDef
        pc'         = instantiate "pc" pcSysDef
        orSetA'     = instantiate "setA:or" orSysDef
        invSetA'    = instantiate "setA:inv" invSysDef
        and' l      = instantiate (l ++ ":and") andSysDef
        decoder'    = instantiate "decoder" decoderSysDef
        -- using the parts
        aReg = aReg' aMux setA
        dReg = dReg' aluOut setD
        aMux = (mux2' "aMux") aFlag instruction aluOut
        am   = (mux2' "am") cAM inM aReg
        nextInst = pc' reset setPC aReg

        (aFlag, cAM, cDest, cJump, cALU) = unzip5SY "unzipDecoder" (decoder' instruction)
        (writeA, writeD, writeM) = unzip3SY "unzipDest" cDest

        (aluOut, aluFlags) = unzipSY "unzipALU" (alu' cALU dReg am)
        setPC = decideJump' cJump aluFlags
        setD = (and' "setD") aFlag writeD
        setA = orSetA' (invSetA' aFlag) ((and' "setA") aFlag writeA)

{-

-- | A Hack instruction is a vector of 16 bits
cpu wordSize (inM, instruction, reset) = (outM, writeM, addressM, pc)
  where
    (i00,i01,i02,i03,i04,i05,i06,i07,i08,i09,i10,i11,i12,i13,i14,i15) = instruction
    insList = [i00,i01,i02,i03,i04,i05,i06,i07,i08,i09,i10,i11,i12,i13,i14,i15]
-}
