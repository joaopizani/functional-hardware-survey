module ALU where

import Lava
import Lava.Patterns


halfAdder :: (Signal Bool, Signal Bool) -> (Signal Bool, Signal Bool)
halfAdder (a, b) = (s, carry)
    where
      s     = xor2 (a, b)
      carry = and2 (a, b)

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

testRippleCarryAdder = simulateSeq rippleCarryAdder input
    where
      input = [[(low,low), (low,low), (low,low), (low,low), (low,low), (low,low), (low,low), (low,low)
              , (low,low), (low,low), (low,low), (low,low), (low,low), (low,low), (low,low), (low,low) ]]
