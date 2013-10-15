module Register where

import Lava


reg :: (Signal Bool, Signal Bool) -> Signal Bool
reg (input, load) = out
  where
    dff = mux (load, (out, input))
    out = delay low dff

testReg1 :: [Signal Bool]
testReg1 = simulateSeq reg inputs
    where
        inputs =
            [ (low, high)
            , (high, low)
            , (high, low)
            , (high, high)
            , (low, low)
            ]
{-
        outputs = [low, low, low, low, high]
-}



regN :: Int -> ([Signal Bool], Signal Bool) -> [Signal Bool]
regN n (input, load) = map reg $ zip input (replicate n load)

testRegN4 :: [[Signal Bool]]
testRegN4 = simulateSeq (regN 4) inputs
    where
        allLow  = replicate 4 low
        allHigh = replicate 4 high
        inputs =
            [ (allLow, high)
            , (allHigh, low)
            , (allHigh, low)
            , (allHigh, high)
            , (allLow, low)
            ]
{-
        outputs = [allLow, allLow, allLow, allLow, allHigh]
-}
