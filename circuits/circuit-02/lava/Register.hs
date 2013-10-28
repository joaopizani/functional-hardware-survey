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


muxWordN :: Int -> (SB, ([SB], [SB])) -> [SB]
muxWordN 0 _                       = []
muxWordN n (s, (w0h:w0t, w1h:w1t)) = mux (s, (w0h, w1h)) : muxWordN (n-1) (s, (w0t, w1t))

mux4WordN :: Int -> ((SB,SB), ([SB],[SB],[SB],[SB])) -> [SB]
mux4WordN n ((sl,sh), (w0,w1,w2,w3)) = m0
    where
        m0  = muxWordN n (sh, (m10, m11))
        m10 = muxWordN n (sl, (w0, w1))
        m11 = muxWordN n (sl, (w2, w3))

mux16WordN :: Int -> ((SB,SB,SB,SB), [[SB]]) -> [SB]  -- 16 inputs, 1 output
mux16WordN n ((s0,s1,s2,s3), w00:w01:w02:w03:w04:w05:w06:w07:w08:w09:w10:w11:w12:w13:w14:w15:_) = m0
    where
        m0  = mux4WordN n ((s2,s3), (m10,m11,m12,m13))
        m10 = mux4WordN n ((s0,s1), (w00,w01,w02,w03))
        m11 = mux4WordN n ((s0,s1), (w04,w05,w06,w07))
        m12 = mux4WordN n ((s0,s1), (w08,w09,w10,w11))
        m13 = mux4WordN n ((s0,s1), (w12,w13,w14,w15))

mux64WordN :: Int -> ((SB,SB,SB,SB,SB,SB), [[SB]]) -> [SB]  -- 64 inputs, 1 output
mux64WordN n ((s0,s1,s2,s3,s4,s5), w0:w1:w2:w3:w4:w5:w6:w7:w8:w9:w10:w11:w12:w13:w14:w15:w16:w17:w18:w19:w20:w21:w22:w23:w24:w25:w26:w27:w28:w29:w30:w31:w32:w33:w34:w35:w36:w37:w38:w39:w40:w41:w42:w43:w44:w45:w46:w47:w48:w49:w50:w51:w52:w53:w54:w55:w56:w57:w58:w59:w60:w61:w62:w63:_) = m0
    where
        m0  = mux4WordN n ((s4,s5), (m10,m11,m12,m13))
        m10 = mux16WordN n ((s0,s1,s2,s3), [w0, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10,w11,w12,w13,w14,w15])
        m11 = mux16WordN n ((s0,s1,s2,s3), [w16,w17,w18,w19,w20,w21,w22,w23,w24,w25,w26,w27,w28,w29,w30,w31])
        m12 = mux16WordN n ((s0,s1,s2,s3), [w32,w33,w34,w35,w36,w37,w38,w39,w40,w41,w42,w43,w44,w45,w46,w47])
        m13 = mux16WordN n ((s0,s1,s2,s3), [w48,w49,w50,w51,w52,w53,w54,w55,w56,w57,w58,w59,w60,w61,w62,w63])


mux4bit :: ((SB, SB), (SB, SB, SB, SB)) -> SB
mux4bit (sel, (i0,i1,i2,i3)) = head $ mux4WordN 1 (sel, ([i0],[i1],[i2],[i3]))

mux16bit :: ((SB,SB,SB,SB), [SB]) -> SB  -- 16 inputs, 1 output
mux16bit (sel, ws) = head $ mux16WordN 1 (sel, map (\i -> [i]) ws)

mux64bit :: ((SB,SB,SB,SB,SB,SB), [SB]) -> SB  -- 64 inputs, 1 output
mux64bit (sel, ws) = head $ mux64WordN 1 (sel, map (\i -> [i]) ws)


decode2To4 :: (SB,SB) -> (SB,SB,SB,SB)
decode2To4 (i0,i1) = (inv i0 <&> inv i1, i0 <&> inv i1, inv i0 <&> i1, i0 <&> i1)


decode4To16 :: (SB,SB,SB,SB) -> [SB]
decode4To16 (s0,s1,s2,s3) =
    [ m00 <&> m10, m00 <&> m11, m00 <&> m12, m00 <&> m13
    , m01 <&> m10, m01 <&> m11, m01 <&> m12, m01 <&> m13
    , m02 <&> m10, m02 <&> m11, m02 <&> m12, m02 <&> m13
    , m03 <&> m10, m03 <&> m11, m03 <&> m12, m03 <&> m13 ]
    where
        (m00,m01,m02,m03) = decode2To4 (s0,s1)
        (m10,m11,m12,m13) = decode2To4 (s2,s3)


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



ram16Rows :: Int -> ([SB], (SB,SB,SB,SB), SB) -> [SB]
ram16Rows n (input, addr, load) = mux16WordN n (addr, registers)
    where
        memLine sel = regN n (input, sel <&> load)
        registers = map memLine (decode4To16 addr)


