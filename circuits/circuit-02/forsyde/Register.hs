{-# LANGUAGE TemplateHaskell #-}
module Register where

import ForSyDe

import Data.Param.FSVec hiding ((++))
import qualified Data.Param.FSVec as V

import Prelude hiding (not)
import Data.TypeLevel.Num hiding ((==))
import Data.Int (Int16)
import Data.Bits ((.&.))


type WordType = Int16


-- BIG disadvantage of ForSyDe: name managing by hand

reg :: Signal WordType -> Signal Bit -> Signal WordType
reg input load = out
    where
        out = delaySY "delay" (0 :: WordType) dff
        dff = (instantiate "mux2" mux2SysDef) load out input

regSysDef :: SysDef (Signal WordType -> Signal Bit -> Signal WordType)
regSysDef = newSysDef reg "regSysDef" ["input", "load"] ["out"]


-- In the mux circuits, the selection bit vectors have the least-significant bit on the LEFT.

mux2 :: Signal Bit -> Signal WordType -> Signal WordType -> Signal WordType
mux2 = zipWith3SY "zipWith3SY" $(newProcFun [d| f :: Bit -> WordType -> WordType -> WordType
                                                f s x y = if s == L then x else y |])

mux2SysDef :: SysDef (Signal Bit -> Signal WordType -> Signal WordType -> Signal WordType)
mux2SysDef = newSysDef mux2 "mux2Sys" ["sel", "in1", "in2"] ["out"]


mux4 :: Signal (FSVec D2 Bit) -> Signal (FSVec D4 WordType) -> Signal WordType
mux4 ss is = (mux2' "m1") (sv ! d1) m00 m01
    where
        mux2' l = instantiate l mux2SysDef
        sv      = unzipxSY "unzipSel" ss
        iv      = unzipxSY "unzipInp" is
        m00     = (mux2' "m00") (sv ! d0) (iv ! d0) (iv ! d1)
        m01     = (mux2' "m01") (sv ! d0) (iv ! d2) (iv ! d3)

mux4SysDef :: SysDef (Signal (FSVec D2 Bit) -> Signal (FSVec D4 WordType) -> Signal WordType)
mux4SysDef = newSysDef mux4 "mux4Sys" ["sel", "inputs"] ["out"]


mux16 :: Signal (FSVec D4 Bit) -> Signal (FSVec D16 WordType) -> Signal WordType
mux16 ss is = (mux4' "m1") sv1 $ zipxSY "zipMs" (m00 +> m01 +> m02 +> m03 +> empty)
    where
        mux4' l = instantiate l mux4SysDef
        sv'     = unzipxSY "gunzipSel" ss
        iv'     = unzipxSY "gunzipInp" is
        sv0     = zipxSY "grezipSel0" (V.select d0 d1 d2 sv')
        sv1     = zipxSY "grezipSel1" (V.select d2 d1 d2 sv')
        iv0     = zipxSY "grezipInp0" (V.select d0 d1 d4 iv')
        iv1     = zipxSY "grezipInp1" (V.select d4 d1 d4 iv')
        iv2     = zipxSY "grezipInp2" (V.select d8 d1 d4 iv')
        iv3     = zipxSY "grezipInp3" (V.select d12 d1 d4 iv')
        m00     = (mux4' "gm00") sv0 iv0  -- TODO: map with zipped names and see VHDL
        m01     = (mux4' "gm01") sv0 iv1
        m02     = (mux4' "gm02") sv0 iv2
        m03     = (mux4' "gm03") sv0 iv3

mux16SysDef :: SysDef (Signal (FSVec D4 Bit) -> Signal (FSVec D16 WordType) -> Signal WordType)
mux16SysDef = newSysDef mux16 "mux16Sys" ["sel", "inputs"] ["out"]


mux64 :: Signal (FSVec D6 Bit) -> Signal (FSVec D64 WordType) -> Signal WordType
mux64 ss is = (instantiate "m1" mux4SysDef) sv1 $ zipxSY "zipMs" (m00 +> m01 +> m02 +> m03 +> empty)
    where
        mux16' l = instantiate l mux16SysDef
        sv'      = unzipxSY "unzipSel" ss
        iv'      = unzipxSY "unzipInp" is
        sv0      = zipxSY "rezipSel0" (V.select d0 d1 d4 sv')
        sv1      = zipxSY "rezipSel1" (V.select d4 d1 d2 sv')
        iv0      = zipxSY "rezipInp0" (V.select d0  d1 d16 iv')  -- TODO: parameterize VHDL
        iv1      = zipxSY "rezipInp1" (V.select d16 d1 d16 iv')
        iv2      = zipxSY "rezipInp2" (V.select d32 d1 d16 iv')
        iv4      = zipxSY "rezipInp3" (V.select d48 d1 d16 iv')
        m00      = (mux16' "m00") sv0 iv0  -- TODO: map with zipped names and see VHDL
        m01      = (mux16' "m01") sv0 iv1
        m02      = (mux16' "m02") sv0 iv2
        m03      = (mux16' "m03") sv0 iv4

mux64SysDef :: SysDef (Signal (FSVec D6 Bit) -> Signal (FSVec D64 WordType) -> Signal WordType)
mux64SysDef = newSysDef mux64 "mux64Sys" ["sel", "inputs"] ["out"]


decode6To64 :: Signal (FSVec D6 Bit) -> Signal (FSVec D64 Bit)
decode6To64 = mapSY "mapSYdecode" decode6To64'
 where
  decode6To64' =
   $(newProcFun
      [d|
       -- Simply output all minterms
       f :: FSVec D6 Bit -> FSVec D64 Bit
       f i =
                not (i!d0) .&. not (i!d1) .&. not (i!d2) .&. not (i!d3) .&. not (i!d4) .&. not (i!d5)
             +> not (i!d0) .&. not (i!d1) .&. not (i!d2) .&. not (i!d3) .&. not (i!d4) .&.     (i!d5)
             +> not (i!d0) .&. not (i!d1) .&. not (i!d2) .&. not (i!d3) .&.     (i!d4) .&. not (i!d5)
             +> not (i!d0) .&. not (i!d1) .&. not (i!d2) .&. not (i!d3) .&.     (i!d4) .&.     (i!d5)
             +> not (i!d0) .&. not (i!d1) .&. not (i!d2) .&.     (i!d3) .&. not (i!d4) .&. not (i!d5)
             +> not (i!d0) .&. not (i!d1) .&. not (i!d2) .&.     (i!d3) .&. not (i!d4) .&.     (i!d5)
             +> not (i!d0) .&. not (i!d1) .&. not (i!d2) .&.     (i!d3) .&.     (i!d4) .&. not (i!d5)
             +> not (i!d0) .&. not (i!d1) .&. not (i!d2) .&.     (i!d3) .&.     (i!d4) .&.     (i!d5)
             +> not (i!d0) .&. not (i!d1) .&.     (i!d2) .&. not (i!d3) .&. not (i!d4) .&. not (i!d5)
             +> not (i!d0) .&. not (i!d1) .&.     (i!d2) .&. not (i!d3) .&. not (i!d4) .&.     (i!d5)
             +> not (i!d0) .&. not (i!d1) .&.     (i!d2) .&. not (i!d3) .&.     (i!d4) .&. not (i!d5)
             +> not (i!d0) .&. not (i!d1) .&.     (i!d2) .&. not (i!d3) .&.     (i!d4) .&.     (i!d5)
             +> not (i!d0) .&. not (i!d1) .&.     (i!d2) .&.     (i!d3) .&. not (i!d4) .&. not (i!d5)
             +> not (i!d0) .&. not (i!d1) .&.     (i!d2) .&.     (i!d3) .&. not (i!d4) .&.     (i!d5)
             +> not (i!d0) .&. not (i!d1) .&.     (i!d2) .&.     (i!d3) .&.     (i!d4) .&. not (i!d5)
             +> not (i!d0) .&. not (i!d1) .&.     (i!d2) .&.     (i!d3) .&.     (i!d4) .&.     (i!d5)

             +> not (i!d0) .&.     (i!d1) .&. not (i!d2) .&. not (i!d3) .&. not (i!d4) .&. not (i!d5)
             +> not (i!d0) .&.     (i!d1) .&. not (i!d2) .&. not (i!d3) .&. not (i!d4) .&.     (i!d5)
             +> not (i!d0) .&.     (i!d1) .&. not (i!d2) .&. not (i!d3) .&.     (i!d4) .&. not (i!d5)
             +> not (i!d0) .&.     (i!d1) .&. not (i!d2) .&. not (i!d3) .&.     (i!d4) .&.     (i!d5)
             +> not (i!d0) .&.     (i!d1) .&. not (i!d2) .&.     (i!d3) .&. not (i!d4) .&. not (i!d5)
             +> not (i!d0) .&.     (i!d1) .&. not (i!d2) .&.     (i!d3) .&. not (i!d4) .&.     (i!d5)
             +> not (i!d0) .&.     (i!d1) .&. not (i!d2) .&.     (i!d3) .&.     (i!d4) .&. not (i!d5)
             +> not (i!d0) .&.     (i!d1) .&. not (i!d2) .&.     (i!d3) .&.     (i!d4) .&.     (i!d5)
             +> not (i!d0) .&.     (i!d1) .&.     (i!d2) .&. not (i!d3) .&. not (i!d4) .&. not (i!d5)
             +> not (i!d0) .&.     (i!d1) .&.     (i!d2) .&. not (i!d3) .&. not (i!d4) .&.     (i!d5)
             +> not (i!d0) .&.     (i!d1) .&.     (i!d2) .&. not (i!d3) .&.     (i!d4) .&. not (i!d5)
             +> not (i!d0) .&.     (i!d1) .&.     (i!d2) .&. not (i!d3) .&.     (i!d4) .&.     (i!d5)
             +> not (i!d0) .&.     (i!d1) .&.     (i!d2) .&.     (i!d3) .&. not (i!d4) .&. not (i!d5)
             +> not (i!d0) .&.     (i!d1) .&.     (i!d2) .&.     (i!d3) .&. not (i!d4) .&.     (i!d5)
             +> not (i!d0) .&.     (i!d1) .&.     (i!d2) .&.     (i!d3) .&.     (i!d4) .&. not (i!d5)
             +> not (i!d0) .&.     (i!d1) .&.     (i!d2) .&.     (i!d3) .&.     (i!d4) .&.     (i!d5)

             +>     (i!d0) .&. not (i!d1) .&. not (i!d2) .&. not (i!d3) .&. not (i!d4) .&. not (i!d5)
             +>     (i!d0) .&. not (i!d1) .&. not (i!d2) .&. not (i!d3) .&. not (i!d4) .&.     (i!d5)
             +>     (i!d0) .&. not (i!d1) .&. not (i!d2) .&. not (i!d3) .&.     (i!d4) .&. not (i!d5)
             +>     (i!d0) .&. not (i!d1) .&. not (i!d2) .&. not (i!d3) .&.     (i!d4) .&.     (i!d5)
             +>     (i!d0) .&. not (i!d1) .&. not (i!d2) .&.     (i!d3) .&. not (i!d4) .&. not (i!d5)
             +>     (i!d0) .&. not (i!d1) .&. not (i!d2) .&.     (i!d3) .&. not (i!d4) .&.     (i!d5)
             +>     (i!d0) .&. not (i!d1) .&. not (i!d2) .&.     (i!d3) .&.     (i!d4) .&. not (i!d5)
             +>     (i!d0) .&. not (i!d1) .&. not (i!d2) .&.     (i!d3) .&.     (i!d4) .&.     (i!d5)
             +>     (i!d0) .&. not (i!d1) .&.     (i!d2) .&. not (i!d3) .&. not (i!d4) .&. not (i!d5)
             +>     (i!d0) .&. not (i!d1) .&.     (i!d2) .&. not (i!d3) .&. not (i!d4) .&.     (i!d5)
             +>     (i!d0) .&. not (i!d1) .&.     (i!d2) .&. not (i!d3) .&.     (i!d4) .&. not (i!d5)
             +>     (i!d0) .&. not (i!d1) .&.     (i!d2) .&. not (i!d3) .&.     (i!d4) .&.     (i!d5)
             +>     (i!d0) .&. not (i!d1) .&.     (i!d2) .&.     (i!d3) .&. not (i!d4) .&. not (i!d5)
             +>     (i!d0) .&. not (i!d1) .&.     (i!d2) .&.     (i!d3) .&. not (i!d4) .&.     (i!d5)
             +>     (i!d0) .&. not (i!d1) .&.     (i!d2) .&.     (i!d3) .&.     (i!d4) .&. not (i!d5)
             +>     (i!d0) .&. not (i!d1) .&.     (i!d2) .&.     (i!d3) .&.     (i!d4) .&.     (i!d5)

             +>     (i!d0) .&.     (i!d1) .&. not (i!d2) .&. not (i!d3) .&. not (i!d4) .&. not (i!d5)
             +>     (i!d0) .&.     (i!d1) .&. not (i!d2) .&. not (i!d3) .&. not (i!d4) .&.     (i!d5)
             +>     (i!d0) .&.     (i!d1) .&. not (i!d2) .&. not (i!d3) .&.     (i!d4) .&. not (i!d5)
             +>     (i!d0) .&.     (i!d1) .&. not (i!d2) .&. not (i!d3) .&.     (i!d4) .&.     (i!d5)
             +>     (i!d0) .&.     (i!d1) .&. not (i!d2) .&.     (i!d3) .&. not (i!d4) .&. not (i!d5)
             +>     (i!d0) .&.     (i!d1) .&. not (i!d2) .&.     (i!d3) .&. not (i!d4) .&.     (i!d5)
             +>     (i!d0) .&.     (i!d1) .&. not (i!d2) .&.     (i!d3) .&.     (i!d4) .&. not (i!d5)
             +>     (i!d0) .&.     (i!d1) .&. not (i!d2) .&.     (i!d3) .&.     (i!d4) .&.     (i!d5)
             +>     (i!d0) .&.     (i!d1) .&.     (i!d2) .&. not (i!d3) .&. not (i!d4) .&. not (i!d5)
             +>     (i!d0) .&.     (i!d1) .&.     (i!d2) .&. not (i!d3) .&. not (i!d4) .&.     (i!d5)
             +>     (i!d0) .&.     (i!d1) .&.     (i!d2) .&. not (i!d3) .&.     (i!d4) .&. not (i!d5)
             +>     (i!d0) .&.     (i!d1) .&.     (i!d2) .&. not (i!d3) .&.     (i!d4) .&.     (i!d5)
             +>     (i!d0) .&.     (i!d1) .&.     (i!d2) .&.     (i!d3) .&. not (i!d4) .&. not (i!d5)
             +>     (i!d0) .&.     (i!d1) .&.     (i!d2) .&.     (i!d3) .&. not (i!d4) .&.     (i!d5)
             +>     (i!d0) .&.     (i!d1) .&.     (i!d2) .&.     (i!d3) .&.     (i!d4) .&. not (i!d5)
             +>     (i!d0) .&.     (i!d1) .&.     (i!d2) .&.     (i!d3) .&.     (i!d4) .&.     (i!d5)
             +> empty
      |])

decode6To64SysDef :: SysDef (Signal (FSVec D6 Bit) -> Signal (FSVec D64 Bit))
decode6To64SysDef = newSysDef decode6To64 "decode6To64Sys" ["ins"] ["outs"]


andProc :: Signal Bit -> Signal Bit -> Signal Bit
andProc = zipWithSY "zipWithSY" $(newProcFun [d| f :: Bit -> Bit -> Bit
                                                 f x y = x .&. y |])

andSysDef :: SysDef (Signal Bit -> Signal Bit -> Signal Bit)
andSysDef = newSysDef andProc "andSysDef" ["in1", "in2"] ["out"]


ram64 :: Signal WordType -> Signal (FSVec D6 Bit) -> Signal Bit -> Signal WordType
ram64 input addr load = mux' addr (zipxSY "zipRows" rs)
    where
        -- parts declarations
        mux'     = instantiate "mux" mux64SysDef
        decoder' = instantiate "decoder" decode6To64SysDef
        reg' l   = instantiate l regSysDef
        and' l   = instantiate l andSysDef
        -- using the parts
        r (s, l) = (reg' l) input ((and' (l ++ ":and")) load s)
        rs       = V.map r $ V.zip rs' (V.map (\n -> "r" ++ show n) (V.unsafeVector d64 [0..63]))
        rs'      = unzipxSY "unzipAddr" $ decoder' addr

ram64SysDef :: SysDef (Signal WordType -> Signal (FSVec D6 Bit) -> Signal Bit -> Signal WordType)
ram64SysDef = newSysDef ram64 "ram64Sys" ["input", "addr", "load"] ["outWord"]

