module ALU where

import Lava
import Lava.Patterns


halfAdder :: (Signal Bool, Signal Bool) -> (Signal Bool, Signal Bool)
halfAdder inputs = (xor2 inputs, and2 inputs)

verifyHalfAdder :: [(Signal Bool, Signal Bool)]
verifyHalfAdder = simulateSeq halfAdder input
    where
      input = [ (low,  low)
              , (low,  high)
              , (high, low)
              , (high, high) ]


fullAdder :: (Signal Bool, (Signal Bool, Signal Bool)) -> (Signal Bool, Signal Bool)
fullAdder (cin, (a, b)) = (s, cout)
    where
      (ab, c1) = halfAdder (a, b)
      (s, c2)  = halfAdder (ab, cin)
      cout     = or2 (c1, c2)


verifyFullAdder :: [(Signal Bool, Signal Bool)]
verifyFullAdder = simulateSeq fullAdder input
    where
      input = [ (low,  (low,  low))
              , (low,  (low,  high))
              , (low,  (high, low))
              , (low,  (high, high))
              , (high, (low,  low))
              , (high, (low,  high))
              , (high, (high, low))
              , (high, (high, high)) ]


rippleCarryAdder :: [(Signal Bool, Signal Bool)] -> [Signal Bool]
rippleCarryAdder ab = s
    where (s, _) = row fullAdder (low, ab)

testRippleCarryAdder :: [[Signal Bool]]
testRippleCarryAdder = simulateSeq rippleCarryAdder input
    where
      input = [[(low,low), (low,low), (low,low), (low,low), (low,low), (low,low), (low,low), (low,low)
              , (low,low), (low,low), (low,low), (low,low), (low,low), (low,low), (low,low), (low,low) ]]


andLifted :: [(Signal Bool, Signal Bool)] -> [Signal Bool]
andLifted = map and2


increment :: [Signal Bool] -> [Signal Bool]
increment a = s
    where (s, _) = row fullAdder (high, zip a (repeat high))


-- Had to group single-bit inputs separately because Lava doesn't provide a Generic
-- instance for tuples with more than 6 elements
-- ASSUMPTION: x and l have the same length
alu :: ( [Signal Bool], [Signal Bool]  -- numerical inputs
      , (Signal Bool, Signal Bool, Signal Bool, Signal Bool, Signal Bool, Signal Bool))  -- control
      -> ([Signal Bool], Signal Bool, Signal Bool)
alu (x, y, (zx, nx, zy, ny, f, no)) = (out', zr, ng)
    where
      out'        = ifThenElse no (out, map inv out)
      zr          = foldl (curry or2) low out'
      ng          = equalBool low (last out')
      out         = let xy'' = zip x'' y'' in mux (f, (andLifted xy'', rippleCarryAdder xy''))
      x'          = ifThenElse zx (x, replicate (length x) low)
      x''         = ifThenElse nx (x', map inv x')
      y'          = ifThenElse zy (y, replicate (length x) low)
      y''         = ifThenElse ny (y', map inv y')


testALU :: [([Signal Bool], Signal Bool, Signal Bool)]
testALU = simulateSeq alu inputs
    where
      low16  = replicate 16 low
      high16 = replicate 16 high
      inputs =
        [ (low16, high16, (high, low,  high, low,  high, low))
        , (low16, high16, (high, high, high, high, high, high))
        , (low16, high16, (low,  low,  high, high, low,  low))
        , (low16, high16, (high, high, high, low, high, low))
        , (low16, high16, (low, low, high, high, low, low))
        , (low16, high16, (high, high, low, low, low, low))
        , (low16, high16, (high, high, low, low, low, low))
        , (low16, high16, (low, low, high, high, low, high))
        , (low16, high16, (high, high, low, low, low, high))
        , (low16, high16, (low, low, high, high, high, high))
        , (low16, high16, (high, high, low, low, high, high))
        , (low16, high16, (low, high, high, high, high, high))
        , (low16, high16, (high, high, low, high, high, high))
        , (low16, high16, (low, low, high, high, high, low))
        , (low16, high16, (high, high, low, low, high, low))
        , (low16, high16, (low, low, low, low, high, low))
        , (low16, high16, (low, high, low, low, high, high))
        , (low16, high16, (low, low, low, high, high, high))
        , (low16, high16, (low, low, low, low, low, low))
        , (low16, high16, (low, high, low, high, low, high))
        ]

