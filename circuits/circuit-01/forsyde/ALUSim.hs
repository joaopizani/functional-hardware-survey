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

inv :: WordType -> WordType
inv = complement


aluFunc :: ProcFun (WordType -> WordType -> ALUControl -> (WordType, ALUFlags))
aluFunc =
    $(newProcFun
        [d|
            aluFunc :: WordType -> WordType -> ALUControl -> (WordType, ALUFlags)
            aluFunc x y (zx, nx, zy, ny, f, no) = (outn, (zr, ng))
                where
                    (xz, yz) = (if bo zx then 0 else x,       if bo zy then 0 else y)
                    (xn, yn) = (if bo nx then inv xz else xz, if bo ny then inv yz else yz)
                    out      = case f of { ALUSum -> xn + yn;  ALUAnd -> xn .&. yn; }
                    outn     = if bo no then inv out else out
                    zr       = bb (outn == 0)
                    ng       = bb (outn < 0)
        |]
    )

aluProc :: Signal WordType -> Signal WordType -> Signal ALUControl -> Signal (WordType, ALUFlags)
aluProc = zipWith3SY "aluProc" aluFunc

aluSysDef :: SysDef (Signal WordType -> Signal WordType -> Signal ALUControl -> Signal (WordType, ALUFlags))
aluSysDef = newSysDef aluProc "alu" ["x", "y", "ctrl"] ["outs"]


