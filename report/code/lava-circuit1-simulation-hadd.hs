testHalfAdder :: [(SB, SB)]
testHalfAdder = map (simulate halfAdder) input
    where input = [ (low,  low)
                  , (low,  high)
                  , (high, low)
                  , (high, high) ]
