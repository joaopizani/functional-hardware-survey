programCounter :: Int -> (SB, SB, [SB]) -> [SB]
programCounter n (reset, set, input) = out
    where incr     = increment out
          out      = delay (replicate n low) increset
          incinput = mux (set, (incr, input))
          increset = mux (reset, (incinput, replicate n low))
