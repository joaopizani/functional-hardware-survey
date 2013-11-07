{-# LANGUAGE TemplateHaskell #-}
module ALUSyn where

import ForSyDe
import ForSyDe.Bit (bitToBool, boolToBit)
import Data.Bits
import Data.Int (Int16)


-- | The type of the operands AND output of the ALU. Must be a fixed-size integer
type WordType = Int16

-- | Operation type. Low means Sum, High means bitwise And
type ALUOp = Bit

-- | Control bits of the ALU: zero X, negate X, zero Y, negate Y, (sum=1, and=0), negate out
type ALUControl = (Bit, Bit, Bit, Bit, ALUOp, Bit)

-- | Output flags: whether out is zero, whether out is negative
type ALUFlags = (Bit, Bit)



zProc :: ProcId -> Signal Bit -> Signal WordType -> Signal WordType
zProc name = zipWithSY name $(newProcFun [d| f :: Bit -> WordType -> WordType
                                             f z w = if z == H then 0 else w |])

-- complement doesn't work, magic number 42 instead. Investigate why complement is out of scope
nProc :: ProcId -> Signal Bit -> Signal WordType -> Signal WordType
nProc name = zipWithSY name $(newProcFun [d| f :: Bit -> WordType -> WordType
                                             f n w = if n == H then 42 else w |])

compProc :: Signal ALUOp -> Signal WordType -> Signal WordType -> Signal WordType
compProc = zipWith3SY "compProc" $(newProcFun [d| f :: ALUOp -> WordType -> WordType -> WordType
                                                  f o x y = if o == H then x + y else x .&. y |])

tzProc :: Signal WordType -> Signal Bit
tzProc = mapSY "tzProc" $(newProcFun [d| f :: WordType -> Bit
                                         f w = if w == 0 then H else L |])

tnProc :: Signal WordType -> Signal Bit
tnProc = mapSY "tnProc" $(newProcFun [d| f :: WordType -> Bit
                                         f w = if w < 0 then H else L |])


aluProc :: Signal ALUControl -> Signal WordType -> Signal WordType -> Signal (WordType, ALUFlags)
aluProc c x y = zipSY "aluProc" out (zipSY "flagsProc" (tzProc out) (tnProc out))
    where
        (zx,nx,zy,ny,f,no) = unzip6SY "ctrlProc" c
        comp = compProc f (nProc "nx" nx $ zProc "zx" zx $ x) (nProc "ny" ny $ zProc "zy" zy $ y)
        out  = nProc "no" no comp


aluSysDef :: SysDef (Signal ALUControl -> Signal WordType -> Signal WordType -> Signal (WordType, ALUFlags))
aluSysDef = newSysDef aluProc "alu" ["ctrl", "x", "y"] ["outs"]


-- TESTING using simulation
aluSim :: [ALUControl] -> [WordType] -> [WordType] -> [(WordType, ALUFlags)]
aluSim = simulate aluSysDef

aluTest1 = aluSim ctrls xs ys
    where
        ctrls = [ (H,L,H,L,H,L),  (H,H,H,H,H,H),  (H,H,H,L,H,L),  (L,L,H,H,L,L)
                , (H,H,L,L,L,L),  (L,L,H,H,L,H),  (H,H,L,L,L,H),  (L,L,H,H,H,H)
                , (H,H,L,L,H,H),  (L,H,H,H,H,H),  (H,H,L,H,H,H),  (L,L,H,H,H,L)
                , (H,H,L,L,H,L),  (L,L,L,L,H,L),  (L,H,L,L,H,H),  (L,L,L,H,H,H)
                , (L,L,L,L,L,L),  (L,H,L,H,L,H) ]
        xs   = replicate (length ctrls) 0
        ys   = replicate (length ctrls) (negate 1)
        outs = [ (0,  (H,L)), (1,  (L,L)), (-1, (L,H)), (0,  (H,L))
               , (-1, (L,H)), (-1, (L,H)), (0,  (H,L)), (0,  (H,L))
               , (1,  (L,L)), (1,  (L,L)), (0,  (H,L)), (-1, (L,H))
               , (-2, (L,H)), (-1, (L,H)), (1,  (L,L)), (-1, (L,H))
               , (0,  (H,L)), (-1, (L,H)) ]

