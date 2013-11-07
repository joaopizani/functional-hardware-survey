{-# LANGUAGE TemplateHaskell #-}
module Register where

import ForSyDe

import Data.Param.FSVec hiding ((++))
import qualified Data.Param.FSVec as V

import Data.TypeLevel.Num hiding ((==))
import Data.Int (Int16)


type WordType = Int16


-- BIG disadvantage of ForSyDe: name managing by hand

reg :: String -> Signal WordType -> Signal Bit -> Signal WordType
reg name input load = out
    where
        out = delaySY name (0 :: WordType) dff
        dff = mux (name ++ ":M") load out input


-- In the mux circuits, the selection bit vectors have the least-significant bit on the LEFT.

mux :: ProcId -> Signal Bit -> Signal WordType -> Signal WordType -> Signal WordType
mux l = zipWith3SY l $(newProcFun [d| f :: Bit -> WordType -> WordType -> WordType
                                      f s x y = if s == L then x else y |])

mux4 :: ProcId -> Signal (FSVec D2 Bit) -> Signal (FSVec D4 WordType) -> Signal WordType
mux4 l ss is = mux (l ++ ":m1")  (sv ! d1) m00 m01
    where
        sv = unzipxSY (l ++ ":unzipSel") ss
        iv = unzipxSY (l ++ ":unzipInp") is
        m00 = mux (l ++ ":m00") (sv ! d0) (iv ! d0) (iv ! d1)
        m01 = mux (l ++ ":m01") (sv ! d0) (iv ! d2) (iv ! d3)

mux4SysDef :: SysDef (Signal (FSVec D2 Bit) -> Signal (FSVec D4 WordType) -> Signal WordType)
mux4SysDef = newSysDef (mux4 "muxA") "mux4Sys" ["sel", "inputs"] ["out"]


mux16 :: ProcId -> Signal (FSVec D4 Bit) -> Signal (FSVec D16 WordType) -> Signal WordType
mux16 l ss is = mux4 (l ++ ":m1") sv1 $ zipxSY (l ++ ":zipMs") (m00 +> m01 +> m02 +> m03 +> empty)
    where
        sv' = unzipxSY (l ++ ":unzipSel") ss
        iv' = unzipxSY (l ++ ":unzipInp") is
        sv0 = zipxSY (l ++ ":rezipSel0") (V.select d0 d1 d2 sv')
        sv1 = zipxSY (l ++ ":rezipSel1") (V.select d2 d1 d2 sv')
        iv0 = zipxSY (l ++ ":rezipInp0") (V.select d0 d1 d4 iv')
        iv1 = zipxSY (l ++ ":rezipInp1") (V.select d4 d1 d4 iv')
        iv2 = zipxSY (l ++ ":rezipInp2") (V.select d8 d1 d4 iv')
        iv3 = zipxSY (l ++ ":rezipInp3") (V.select d12 d1 d4 iv')
        m00 = mux4 (l ++ ":m00") sv0 iv0  -- TODO: map with zipped names and see VHDL
        m01 = mux4 (l ++ ":m01") sv0 iv1
        m02 = mux4 (l ++ ":m02") sv0 iv2
        m03 = mux4 (l ++ ":m03") sv0 iv3

mux16SysDef :: SysDef (Signal (FSVec D4 Bit) -> Signal (FSVec D16 WordType) -> Signal WordType)
mux16SysDef = newSysDef (mux16 "muxA") "mux16Sys" ["sel", "inputs"] ["out"]


mux64 :: ProcId -> Signal (FSVec D6 Bit) -> Signal (FSVec D64 WordType) -> Signal WordType
mux64 l ss is = mux4 (l ++ ":m1") sv1 $ zipxSY (l ++ ":zipMs") (m00 +> m01 +> m02 +> m03 +> empty)
    where
        sv' = unzipxSY (l ++ ":unzipSel") ss
        iv' = unzipxSY (l ++ ":unzipInp") is
        sv0 = zipxSY (l ++ ":rezipSel0") (V.select d0 d1 d4 sv')
        sv1 = zipxSY (l ++ ":rezipSel1") (V.select d4 d1 d2 sv')
        iv0 = zipxSY (l ++ ":rezipInp0") (V.select d0  d1 d16 iv')  -- TODO: parameterize VHDL
        iv1 = zipxSY (l ++ ":rezipInp1") (V.select d16 d1 d16 iv')
        iv2 = zipxSY (l ++ ":rezipInp2") (V.select d32 d1 d16 iv')
        iv4 = zipxSY (l ++ ":rezipInp3") (V.select d48 d1 d16 iv')
        m00 = mux16 (l ++ ":m00") sv0 iv0  -- TODO: map with zipped names and see VHDL
        m01 = mux16 (l ++ ":m01") sv0 iv1
        m02 = mux16 (l ++ ":m02") sv0 iv2
        m03 = mux16 (l ++ ":m03") sv0 iv4

mux64SysDef :: SysDef (Signal (FSVec D6 Bit) -> Signal (FSVec D64 WordType) -> Signal WordType)
mux64SysDef = newSysDef (mux64 "muxA") "mux64Sys" ["sel", "inputs"] ["out"]


ram64 :: Signal WordType -> Signal (FSVec D6 Bit) -> Signal Bit -> Signal WordType
ram64 = undefined



{-
ram64Rows :: Int -> ([SB], (SB,SB,SB,SB,SB,SB), SB) -> [SB]
ram64Rows n (input, addr, load) = mux64WordN n (addr, registers)
    where
        memLine sel = regN n (input, sel <&> load)
        registers = map memLine (decode6To64 addr)


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


-}
