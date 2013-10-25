module Register where

import Lava


type SB = Signal Bool


reg :: (Signal Bool, Signal Bool) -> Signal Bool
reg (input, load) = out
  where
    dff = mux (load, (out, input))
    out = delay low dff

testReg1 :: [Signal Bool]
testReg1 = simulateSeq reg inputs
    where
        inputs =
            [ (low, high)
            , (high, low)
            , (high, low)
            , (high, high)
            , (low, low)
            ]
{-
        outputs = [low, low, low, low, high]
-}


regN :: Int -> ([Signal Bool], Signal Bool) -> [Signal Bool]
regN n (input, load) = map reg $ zip input (replicate n load)

testRegN4 :: [[Signal Bool]]
testRegN4 = simulateSeq (regN 4) inputs
    where
        allLow  = replicate 4 low
        allHigh = replicate 4 high
        inputs =
            [ (allLow, high)
            , (allHigh, low)
            , (allHigh, low)
            , (allHigh, high)
            , (allLow, low)
            ]
{-
        outputs = [allLow, allLow, allLow, allLow, allHigh]
-}


mux4bit :: ((SB, SB), (SB, SB, SB, SB)) -> SB
mux4bit ((sl,sh), (i0,i1,i2,i3)) = m0
    where
        m0  = mux (sh, (m10, m11))
        m10 = mux (sl, (i0, i1))
        m11 = mux (sl, (i2, i3))

mux16bit :: ((SB,SB,SB,SB), [SB]) -> SB  -- 16 inputs, 1 output
mux16bit ((s0,s1,s2,s3), i00:i01:i02:i03:i04:i05:i06:i07:i08:i09:i10:i11:i12:i13:i14:i15:_) = m0
    where
        m0  = mux4bit ((s2,s3), (m10,m11,m12,m13))
        m10 = mux4bit ((s0,s1), (i00,i01,i02,i03))
        m11 = mux4bit ((s0,s1), (i04,i05,i06,i07))
        m12 = mux4bit ((s0,s1), (i08,i09,i10,i11))
        m13 = mux4bit ((s0,s1), (i12,i13,i14,i15))



