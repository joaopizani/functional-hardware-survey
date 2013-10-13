module Register where

import Lava


reg :: (Signal Bool, Signal Bool) -> Signal Bool
reg (input, load) = out
  where
    dff = mux (load, (out, input))
    out = delay low dff


regN :: Int -> ([Signal Bool], Signal Bool) -> [Signal Bool]
regN n (input, load) = map reg $ zip input (replicate n load)

