reg :: (SB, SB) -> SB
reg (input, load) = out
    where dff = mux (load, (out, input))
          out = delay low dff

regN :: Int -> ([SB], SB) -> [SB]
regN n (input, load) = map reg $ zip input (replicate n load)

ram64Rows :: Int -> ([SB], (SB,SB,SB,SB,SB,SB), SB) -> [SB]
ram64Rows n (input, addr, load) = mux64WordN n (addr, regs)
    where memLine sel = regN n (input, sel <&> load)
          regs        = map memLine (decode6To64 addr)
