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
bo :: Bit -> Bool
bo = bitToBool

bb :: Bool -> Bit
bb = boolToBit


ff :: ALUOp -> WordType -> WordType -> WordType
ff f x y = case f of {ALUSum -> x + y;  ALUAnd -> x .&. y; }

fz :: Bit -> WordType -> WordType
fz z w = if bo z then 0 else w

fn :: Bit -> WordType -> WordType
fn n w = if bo n then complement w else w

tz :: WordType -> Bit
tz w = bb (w == 0)

tn :: WordType -> Bit
tn w = bb (w < 0)

aluFunc :: ProcFun (WordType -> WordType -> ALUControl -> (WordType, ALUFlags))
aluFunc =
    $(newProcFun
        [d|
            aluFunc :: WordType -> WordType -> ALUControl -> (WordType, ALUFlags)
            aluFunc x y (zx, nx, zy, ny, f, no) =
                ( fn no  (ff f (fn nx (fz zx x)) (fn ny (fz zy y)) )
                , ( tz $ fn no  (ff f (fn nx (fz zx x)) (fn ny (fz zy y)) )
                  , tn $ fn no  (ff f (fn nx (fz zx x)) (fn ny (fz zy y)) )
                  )
                )
        |]
    )

aluProc :: Signal WordType -> Signal WordType -> Signal ALUControl -> Signal (WordType, ALUFlags)
aluProc = zipWith3SY "aluProc" aluFunc

aluSysDef :: SysDef (Signal WordType -> Signal WordType -> Signal ALUControl -> Signal (WordType, ALUFlags))
aluSysDef = newSysDef aluProc "alu" ["x", "y", "ctrl"] ["outs"]


