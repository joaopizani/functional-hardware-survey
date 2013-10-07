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
alu :: ( [(Signal Bool, Signal Bool)]  -- numerical inputs
      , (Signal Bool, Signal Bool, Signal Bool, Signal Bool, Signal Bool, Signal Bool))  -- control
      -> ([Signal Bool], Signal Bool, Signal Bool)
alu (xy, (zx, nx, zy, ny, f, no)) = (out', zr, ng)
    where
      ((x, y), l) = (unzip xy, length x)
      out'        = ifThenElse no (out, map inv out)
      zr          = foldl (curry or2) low out'
      ng          = equalBool low (last out')
      out         = let xy'' = zip x'' y'' in mux (f, (andLifted xy'', rippleCarryAdder xy''))
      x'          = ifThenElse zx (x, replicate l low)
      x''         = ifThenElse nx (x', map inv x')
      y'          = ifThenElse zy (y, replicate l low)
      y''         = ifThenElse ny (y', map inv y')


testALU :: [([Signal Bool], Signal Bool, Signal Bool)]
testALU = simulateSeq alu inputs
    where
      lowHigh16 = zip (replicate 16 low) (replicate 16 high)
      inputs =
        [ (lowHigh16, (high, low,  high, low,  high, low))
        , (lowHigh16, (high, high, high, high, high, high))
        , (lowHigh16, (low,  low,  high, high, low,  low))
        ]
{-
|        x         |        y         |zx |nx |zy |ny | f |no |       out        |zr |ng |
| 0000000000000000 | 1111111111111111 | 1 | 1 | 1 | 0 | 1 | 0 | 1111111111111111 | 0 | 1 |
| 0000000000000000 | 1111111111111111 | 0 | 0 | 1 | 1 | 0 | 0 | 0000000000000000 | 1 | 0 |
| 0000000000000000 | 1111111111111111 | 1 | 1 | 0 | 0 | 0 | 0 | 1111111111111111 | 0 | 1 |
| 0000000000000000 | 1111111111111111 | 0 | 0 | 1 | 1 | 0 | 1 | 1111111111111111 | 0 | 1 |
| 0000000000000000 | 1111111111111111 | 1 | 1 | 0 | 0 | 0 | 1 | 0000000000000000 | 1 | 0 |
| 0000000000000000 | 1111111111111111 | 0 | 0 | 1 | 1 | 1 | 1 | 0000000000000000 | 1 | 0 |
| 0000000000000000 | 1111111111111111 | 1 | 1 | 0 | 0 | 1 | 1 | 0000000000000001 | 0 | 0 |
| 0000000000000000 | 1111111111111111 | 0 | 1 | 1 | 1 | 1 | 1 | 0000000000000001 | 0 | 0 |
| 0000000000000000 | 1111111111111111 | 1 | 1 | 0 | 1 | 1 | 1 | 0000000000000000 | 1 | 0 |
| 0000000000000000 | 1111111111111111 | 0 | 0 | 1 | 1 | 1 | 0 | 1111111111111111 | 0 | 1 |
| 0000000000000000 | 1111111111111111 | 1 | 1 | 0 | 0 | 1 | 0 | 1111111111111110 | 0 | 1 |
| 0000000000000000 | 1111111111111111 | 0 | 0 | 0 | 0 | 1 | 0 | 1111111111111111 | 0 | 1 |
| 0000000000000000 | 1111111111111111 | 0 | 1 | 0 | 0 | 1 | 1 | 0000000000000001 | 0 | 0 |
| 0000000000000000 | 1111111111111111 | 0 | 0 | 0 | 1 | 1 | 1 | 1111111111111111 | 0 | 1 |
| 0000000000000000 | 1111111111111111 | 0 | 0 | 0 | 0 | 0 | 0 | 0000000000000000 | 1 | 0 |
| 0000000000000000 | 1111111111111111 | 0 | 1 | 0 | 1 | 0 | 1 | 1111111111111111 | 0 | 1 |
-}

