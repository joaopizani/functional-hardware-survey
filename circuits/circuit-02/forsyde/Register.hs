{-# LANGUAGE TemplateHaskell #-}
module Register where

import ForSyDe

import Prelude hiding ((++))
import Prelude as P
import Data.Param.FSVec

import Data.TypeLevel.Num
import Data.Int (Int16)


type WordType = Int16


reg :: String -> Signal WordType -> Signal Bit -> Signal WordType
reg name input load = out
    where
        out = delaySY name (0 :: WordType) dff
        dff = mux (name P.++ "M") load out input


mux :: ProcId -> Signal Bit -> Signal WordType -> Signal WordType -> Signal WordType
mux l = zipWith3SY l $(newProcFun [d| f :: Bit -> WordType -> WordType -> WordType
                                      f s x y = if s P.== L then x else y |])

mux64 :: Signal (Bit,Bit,Bit,Bit,Bit,Bit) -> Signal (FSVec D64 WordType) -> Signal WordType
mux64 = undefined



{-

mux64WordN :: Int -> ((SB,SB,SB,SB,SB,SB), [[SB]]) -> [SB]  -- 64 inputs, 1 output
mux64WordN n ((s0,s1,s2,s3,s4,s5), w0:w1:w2:w3:w4:w5:w6:w7:w8:w9:w10:w11:w12:w13:w14:w15:w16:w17:w18:w19:w20:w21:w22:w23:w24:w25:w26:w27:w28:w29:w30:w31:w32:w33:w34:w35:w36:w37:w38:w39:w40:w41:w42:w43:w44:w45:w46:w47:w48:w49:w50:w51:w52:w53:w54:w55:w56:w57:w58:w59:w60:w61:w62:w63:_) = m0
    where
        m0  = mux4WordN n ((s4,s5), (m10,m11,m12,m13))
        m10 = mux16WordN n ((s0,s1,s2,s3), [w0, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10,w11,w12,w13,w14,w15])
        m11 = mux16WordN n ((s0,s1,s2,s3), [w16,w17,w18,w19,w20,w21,w22,w23,w24,w25,w26,w27,w28,w29,w30,w31])
        m12 = mux16WordN n ((s0,s1,s2,s3), [w32,w33,w34,w35,w36,w37,w38,w39,w40,w41,w42,w43,w44,w45,w46,w47])
        m13 = mux16WordN n ((s0,s1,s2,s3), [w48,w49,w50,w51,w52,w53,w54,w55,w56,w57,w58,w59,w60,w61,w62,w63])


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


ram64Rows :: Int -> ([SB], (SB,SB,SB,SB,SB,SB), SB) -> [SB]
ram64Rows n (input, addr, load) = mux64WordN n (addr, registers)
    where
        memLine sel = regN n (input, sel <&> load)
        registers = map memLine (decode6To64 addr)
-}
