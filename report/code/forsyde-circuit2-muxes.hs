mux2 :: Signal Bit -> Signal WordType -> Signal WordType
     -> Signal WordType
mux2 = zipWith3SY "zipWith3SY"
           $(newProcFun [d| f :: Bit -> WordType -> WordType -> WordType
                            f s x y = if s == L then x else y |])

mux2SysDef :: SysDef ( Signal Bit -> Signal WordType -> Signal WordType
                     -> Signal WordType)
mux2SysDef = newSysDef mux2 "mux2Sys" ["sel", "in1", "in2"] ["out"]

mux4 :: Signal (FSVec D2 Bit) -> Signal (FSVec D4 WordType) -> Signal WordType
mux4 ss is = (mux2' "m1") (sv ! d1) m00 m01
    where mux2' l = instantiate l mux2SysDef
          sv      = unzipxSY "unzipSel" ss
          iv      = unzipxSY "unzipInp" is
          m00     = (mux2' "m00") (sv ! d0) (iv ! d0) (iv ! d1)
          m01     = (mux2' "m01") (sv ! d0) (iv ! d2) (iv ! d3)

mux4SysDef :: SysDef ( Signal (FSVec D2 Bit) -> Signal (FSVec D4 WordType)
                     -> Signal WordType)
mux4SysDef = newSysDef mux4 "mux4Sys" ["sel", "inputs"] ["out"]
