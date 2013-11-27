type AddrType = Int16

pc :: Signal Bit -> Signal Bit -> Signal AddrType -> Signal AddrType
pc = scanld3SY "programCounter" nextStateFun 0
  where
    nextStateFun =
        $(newProcFun [d| f :: AddrType -> Bit -> Bit -> AddrType -> AddrType
                         f cur reset set new = if reset == H then 0
                                               else if set == H then new
                                               else cur + 1 |])

pcSysDef :: SysDef ( Signal Bit -> Signal Bit -> Signal AddrType
                  -> Signal AddrType)
pcSysDef = newSysDef pc "pcSys" ["reset", "set", "addr"] ["out"]
