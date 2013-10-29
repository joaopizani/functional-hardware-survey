module Register where

import Lava


type SB = Signal Bool


-- | A 1-bit register, with input and load signals
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


-- | A N-bit wide register, with input and load signals
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


-- Addressing logic for the RAM block

-- | A multiplexer with two N-bit wide inputs
muxWordN :: Int -> (SB, ([SB], [SB])) -> [SB]
muxWordN 0 _                       = []
muxWordN n (s, (w0h:w0t, w1h:w1t)) = mux (s, (w0h, w1h)) : muxWordN (n-1) (s, (w0t, w1t))

-- | A multiplexer with four N-bit wide inputs
mux4WordN :: Int -> ((SB,SB), ([SB],[SB],[SB],[SB])) -> [SB]
mux4WordN n ((sl,sh), (w0,w1,w2,w3)) = m0
    where
        m0  = muxWordN n (sh, (m10, m11))
        m10 = muxWordN n (sl, (w0, w1))
        m11 = muxWordN n (sl, (w2, w3))

-- | A multiplexer with sixteen N-bit wide inputs
mux16WordN :: Int -> ((SB,SB,SB,SB), [[SB]]) -> [SB]
mux16WordN n ((s0,s1,s2,s3), w00:w01:w02:w03:w04:w05:w06:w07:w08:w09:w10:w11:w12:w13:w14:w15:_) = m0
    where
        m0  = mux4WordN n ((s2,s3), (m10,m11,m12,m13))
        m10 = mux4WordN n ((s0,s1), (w00,w01,w02,w03))
        m11 = mux4WordN n ((s0,s1), (w04,w05,w06,w07))
        m12 = mux4WordN n ((s0,s1), (w08,w09,w10,w11))
        m13 = mux4WordN n ((s0,s1), (w12,w13,w14,w15))

-- | A multiplexer with sixty-four N-bit wide inputs
mux64WordN :: Int -> ((SB,SB,SB,SB,SB,SB), [[SB]]) -> [SB]
mux64WordN n ((s0,s1,s2,s3,s4,s5), w0:w1:w2:w3:w4:w5:w6:w7:w8:w9:w10:w11:w12:w13:w14:w15:w16:w17:w18:w19:w20:w21:w22:w23:w24:w25:w26:w27:w28:w29:w30:w31:w32:w33:w34:w35:w36:w37:w38:w39:w40:w41:w42:w43:w44:w45:w46:w47:w48:w49:w50:w51:w52:w53:w54:w55:w56:w57:w58:w59:w60:w61:w62:w63:_) = m0
    where
        m0  = mux4WordN n ((s4,s5), (m10,m11,m12,m13))
        m10 = mux16WordN n ((s0,s1,s2,s3), [w0, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10,w11,w12,w13,w14,w15])
        m11 = mux16WordN n ((s0,s1,s2,s3), [w16,w17,w18,w19,w20,w21,w22,w23,w24,w25,w26,w27,w28,w29,w30,w31])
        m12 = mux16WordN n ((s0,s1,s2,s3), [w32,w33,w34,w35,w36,w37,w38,w39,w40,w41,w42,w43,w44,w45,w46,w47])
        m13 = mux16WordN n ((s0,s1,s2,s3), [w48,w49,w50,w51,w52,w53,w54,w55,w56,w57,w58,w59,w60,w61,w62,w63])


-- | A binary decoder with 6-bit input and 64-bit output
decode6To64 :: (SB,SB,SB,SB,SB,SB) -> [SB]
decode6To64 (s0, s1, s2, s3, s4, s5) = 
    [ s0N <&> s1N <&> s2N <&> s3N <&> s4N <&> s5N,  s0  <&> s1N <&> s2N <&> s3N <&> s4N <&> s5N
    , s0N <&> s1  <&> s2N <&> s3N <&> s4N <&> s5N,  s0  <&> s1  <&> s2N <&> s3N <&> s4N <&> s5N
    , s0N <&> s1N <&> s2  <&> s3N <&> s4N <&> s5N,  s0  <&> s1N <&> s2  <&> s3N <&> s4N <&> s5N
    , s0N <&> s1  <&> s2  <&> s3N <&> s4N <&> s5N,  s0  <&> s1  <&> s2  <&> s3N <&> s4N <&> s5N
    , s0N <&> s1N <&> s2N <&> s3  <&> s4N <&> s5N,  s0  <&> s1N <&> s2N <&> s3  <&> s4N <&> s5N
    , s0N <&> s1  <&> s2N <&> s3  <&> s4N <&> s5N,  s0  <&> s1  <&> s2N <&> s3  <&> s4N <&> s5N
    , s0N <&> s1N <&> s2  <&> s3  <&> s4N <&> s5N,  s0  <&> s1N <&> s2  <&> s3  <&> s4N <&> s5N
    , s0N <&> s1  <&> s2  <&> s3  <&> s4N <&> s5N,  s0  <&> s1  <&> s2  <&> s3  <&> s4N <&> s5N
    , s0N <&> s1N <&> s2N <&> s3N <&> s4  <&> s5N,  s0  <&> s1N <&> s2N <&> s3N <&> s4  <&> s5N
    , s0N <&> s1  <&> s2N <&> s3N <&> s4  <&> s5N,  s0  <&> s1  <&> s2N <&> s3N <&> s4  <&> s5N
    , s0N <&> s1N <&> s2  <&> s3N <&> s4  <&> s5N,  s0  <&> s1N <&> s2  <&> s3N <&> s4  <&> s5N
    , s0N <&> s1  <&> s2  <&> s3N <&> s4  <&> s5N,  s0  <&> s1  <&> s2  <&> s3N <&> s4  <&> s5N
    , s0N <&> s1N <&> s2N <&> s3  <&> s4  <&> s5N,  s0  <&> s1N <&> s2N <&> s3  <&> s4  <&> s5N
    , s0N <&> s1  <&> s2N <&> s3  <&> s4  <&> s5N,  s0  <&> s1  <&> s2N <&> s3  <&> s4  <&> s5N
    , s0N <&> s1N <&> s2  <&> s3  <&> s4  <&> s5N,  s0  <&> s1N <&> s2  <&> s3  <&> s4  <&> s5N
    , s0N <&> s1  <&> s2  <&> s3  <&> s4  <&> s5N,  s0  <&> s1  <&> s2  <&> s3  <&> s4  <&> s5N
    , s0N <&> s1N <&> s2N <&> s3N <&> s4N <&> s5 ,  s0  <&> s1N <&> s2N <&> s3N <&> s4N <&> s5
    , s0N <&> s1  <&> s2N <&> s3N <&> s4N <&> s5 ,  s0  <&> s1  <&> s2N <&> s3N <&> s4N <&> s5
    , s0N <&> s1N <&> s2  <&> s3N <&> s4N <&> s5 ,  s0  <&> s1N <&> s2  <&> s3N <&> s4N <&> s5
    , s0N <&> s1  <&> s2  <&> s3N <&> s4N <&> s5 ,  s0  <&> s1  <&> s2  <&> s3N <&> s4N <&> s5
    , s0N <&> s1N <&> s2N <&> s3  <&> s4N <&> s5 ,  s0  <&> s1N <&> s2N <&> s3  <&> s4N <&> s5
    , s0N <&> s1  <&> s2N <&> s3  <&> s4N <&> s5 ,  s0  <&> s1  <&> s2N <&> s3  <&> s4N <&> s5
    , s0N <&> s1N <&> s2  <&> s3  <&> s4N <&> s5 ,  s0  <&> s1N <&> s2  <&> s3  <&> s4N <&> s5
    , s0N <&> s1  <&> s2  <&> s3  <&> s4N <&> s5 ,  s0  <&> s1  <&> s2  <&> s3  <&> s4N <&> s5
    , s0N <&> s1N <&> s2N <&> s3N <&> s4  <&> s5 ,  s0  <&> s1N <&> s2N <&> s3N <&> s4  <&> s5
    , s0N <&> s1  <&> s2N <&> s3N <&> s4  <&> s5 ,  s0  <&> s1  <&> s2N <&> s3N <&> s4  <&> s5
    , s0N <&> s1N <&> s2  <&> s3N <&> s4  <&> s5 ,  s0  <&> s1N <&> s2  <&> s3N <&> s4  <&> s5
    , s0N <&> s1  <&> s2  <&> s3N <&> s4  <&> s5 ,  s0  <&> s1  <&> s2  <&> s3N <&> s4  <&> s5
    , s0N <&> s1N <&> s2N <&> s3  <&> s4  <&> s5 ,  s0  <&> s1N <&> s2N <&> s3  <&> s4  <&> s5
    , s0N <&> s1  <&> s2N <&> s3  <&> s4  <&> s5 ,  s0  <&> s1  <&> s2N <&> s3  <&> s4  <&> s5
    , s0N <&> s1N <&> s2  <&> s3  <&> s4  <&> s5 ,  s0  <&> s1N <&> s2  <&> s3  <&> s4  <&> s5
    , s0N <&> s1  <&> s2  <&> s3  <&> s4  <&> s5 ,  s0  <&> s1  <&> s2  <&> s3  <&> s4  <&> s5 ]
    where
        (s0N, s1N, s2N, s3N, s4N, s5N) = (inv s0, inv s1, inv s2, inv s3, inv s4, inv s5)



-- | A RAM memory element, with 64 rows, each row being N-bit wide.
ram64Rows :: Int -> ([SB], (SB,SB,SB,SB,SB,SB), SB) -> [SB]
ram64Rows n (input, addr, load) = mux64WordN n (addr, registers)
    where
        memLine sel = regN n (input, sel <&> load)
        registers = map memLine (decode6To64 addr)

