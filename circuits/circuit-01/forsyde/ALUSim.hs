{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module ALUSim where

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


aluFunc :: ProcFun (ALUControl -> WordType -> WordType -> (WordType, ALUFlags))
aluFunc =
    $(newProcFun
        [d|
            aluFunc :: ALUControl -> WordType -> WordType -> (WordType, ALUFlags)
            aluFunc (zx, nx, zy, ny, f, no) x y = (out,  (bb (out == 0), bb (out < 0)) )
                where
                    zf z w = if bo z then 0 else w
                    nf n w = if bo n then complement w else w
                    (xn, yn) = (nf nx $ zf zx $ x,  nf ny $ zf zy $ y)
                    out      = nf no $ case f of
                                           ALUSum -> xn + yn
                                           ALUAnd -> xn .&. yn
        |]
    )

aluProc :: Signal ALUControl -> Signal WordType -> Signal WordType -> Signal (WordType, ALUFlags)
aluProc = zipWith3SY "aluProc" aluFunc

aluSysDef :: SysDef (Signal ALUControl -> Signal WordType -> Signal WordType -> Signal (WordType, ALUFlags))
aluSysDef = newSysDef aluProc "alu" ["ctrl", "x", "y"] ["outs"]

