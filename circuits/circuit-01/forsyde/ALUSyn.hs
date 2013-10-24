{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module ALUSyn where

import ForSyDe
import Data.Bits
import Data.Int (Int16)

import Language.Haskell.TH.Lift (deriveLift1)
import Data.Generics (Data, Typeable)


-- | The type of the operands AND output of the ALU. Must be a fixed-size integer
type WordType = Int16

-- | Enumeration of possible operations that the ALU can perform
data ALUOp = ALUSum | ALUAnd
    deriving (Typeable, Data, Show)

$(deriveLift1 ''ALUOp)

-- | Control bits of the ALU: zero X, negate X, zero Y, negate Y, op, negate out
type ALUControl = (Bit, Bit, Bit, Bit, ALUOp, Bit)

-- | Output flags: whether out is zero, whether out is negative
type ALUFlags = (Bit, Bit)


-- Shorter synonyms, will be used often. Also, we could define here GLOBALLY
-- that signals should be active-low (if we wanted to)
b2bo :: Bit -> Bool
b2bo = bitToBool

bo2b :: Bool -> Bit
bo2b = boolToBit


zProc :: ProcId -> Signal Bit -> Signal WordType -> Signal WordType
zProc name = zipWithSY name zeroFunc
    where zeroFunc = $(newProcFun [d| zeroFunc :: Bit -> WordType -> WordType
                                      zeroFunc z w = if b2bo z then 0 else w |])

nProc :: ProcId -> Signal Bit -> Signal WordType -> Signal WordType
nProc name = zipWithSY name negFunc
    where negFunc = $(newProcFun [d| negFunc :: Bit -> WordType -> WordType
                                     negFunc n w = if b2bo n then complement w else w |])

compProc :: Signal ALUOp -> Signal WordType -> Signal WordType -> Signal WordType
compProc = zipWith3SY "compProc" compFunc
    where compFunc = $(newProcFun [d| compFunc :: ALUOp -> WordType -> WordType -> WordType
                                      compFunc o x y = case o of
                                                           ALUSum -> x + y
                                                           ALUAnd -> x .&. y |])

tzProc :: Signal WordType -> Signal Bit
tzProc = mapSY "tzProc" $(newProcFun [d| tz :: WordType -> Bit
                                         tz w = bo2b (w == 0) |])

tnProc :: Signal WordType -> Signal Bit
tnProc = mapSY "tnProc" $(newProcFun [d| tn :: WordType -> Bit
                                         tn w = bo2b (w < 0) |])

aluProc :: Signal ALUControl -> Signal WordType -> Signal WordType -> Signal (WordType, ALUFlags)
aluProc c x y = zipSY "aluProc" out flags
    where
        (zx,nx,zy,ny,f,no) = unzip6SY "ctrlProc" c
        comp = compProc f (nProc "nx" nx $ zProc "zx" zx $ x) (nProc "ny" ny $ zProc "zy" zy $ y)
        out  = nProc "no" no comp
        flags = zipSY "flagsProc" (tzProc out) (tnProc out)


aluSysDef :: SysDef (Signal ALUControl -> Signal WordType -> Signal WordType -> Signal (WordType, ALUFlags))
aluSysDef = newSysDef aluProc "alu" ["ctrl", "x", "y"] ["outs"]


