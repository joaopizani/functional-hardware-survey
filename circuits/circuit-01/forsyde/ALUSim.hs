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


-- TESTING using simulation
aluSim :: [ALUControl] -> [WordType] -> [WordType] -> [(WordType, ALUFlags)]
aluSim = simulate aluSysDef

aluTest1 :: Bool
aluTest1 = aluSim ctrls xs ys == outs
    where
        ctrls = [ (H,L,H,L,ALUSum,L),  (H,H,H,H,ALUSum,H),  (H,H,H,L,ALUSum,L),  (L,L,H,H,ALUAnd,L)
                , (H,H,L,L,ALUAnd,L),  (L,L,H,H,ALUAnd,H),  (H,H,L,L,ALUAnd,H),  (L,L,H,H,ALUSum,H)
                , (H,H,L,L,ALUSum,H),  (L,H,H,H,ALUSum,H),  (H,H,L,H,ALUSum,H),  (L,L,H,H,ALUSum,L)
                , (H,H,L,L,ALUSum,L),  (L,L,L,L,ALUSum,L),  (L,H,L,L,ALUSum,H),  (L,L,L,H,ALUSum,H)
                , (L,L,L,L,ALUAnd,L),  (L,H,L,H,ALUAnd,H) ]
        xs   = replicate (length ctrls) 0
        ys   = replicate (length ctrls) (negate 1)
        outs = [ (0,  (H,L)), (1,  (L,L)), (-1, (L,H)), (0,  (H,L))
               , (-1, (L,H)), (-1, (L,H)), (0,  (H,L)), (0,  (H,L))
               , (1,  (L,L)), (1,  (L,L)), (0,  (H,L)), (-1, (L,H))
               , (-2, (L,H)), (-1, (L,H)), (1,  (L,L)), (-1, (L,H))
               , (0,  (H,L)), (-1, (L,H)) ]

