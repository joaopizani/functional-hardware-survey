incrFunc :: ProcFun (Int16 -> Int16)
incrFunc = \$(newProcFun [d| f :: Int16 -> Int16
                             f x = x + 1 |])

incrementer :: Signal Int16 -> Signal Int16
incrementer = mapSY "incrementerProc" incrFunc
