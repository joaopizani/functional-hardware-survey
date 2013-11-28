testRegN4 :: [[Signal Bool]]
testRegN4 = simulateSeq (regN 4) ins
    where los = replicate 4 low
          his = replicate 4 high
          ins = [(los,high), (his,low), (his,low), (his,high), (los,low)]
