testRegN4 :: [[Signal Bool]]
testRegN4 = simulateSeq (regN 4) ins
    where
        lows  = replicate 4 low
        highs = replicate 4 high
        ins   = [(lows,high), (highs,low), (highs,low), (highs,high), (lows,low)]
