ram64Rows :: Int -> ([SB], (SB,SB,SB,SB,SB,SB), SB) -> [SB]
ram64Rows n (input, addr, load) = mux64WordN n (addr, registers)
    where memLine sel = regN n (input, sel <&> load)
          registers   = map memLine (decode6To64 addr)
