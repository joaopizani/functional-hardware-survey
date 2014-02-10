reg :: Signal WordType -> Signal Bit -> Signal WordType
reg input load = out
    where out = delaySY "delay" (0 :: WordType) dff
          dff = (instantiate "mux2" mux2SysDef) load out input
