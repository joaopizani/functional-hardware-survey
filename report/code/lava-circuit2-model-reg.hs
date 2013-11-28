reg :: (Signal Bool, Signal Bool) -> Signal Bool
reg (input, load) = out
    where dff = mux (load, (out, input))
          out = delay low dff
