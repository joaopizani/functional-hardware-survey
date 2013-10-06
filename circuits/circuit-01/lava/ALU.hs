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


andLifted :: [(Signal Bool, Signal Bool)] -> [Signal Bool]
andLifted = map and2


increment :: [Signal Bool] -> [Signal Bool]
increment a = s
    where (s, _) = row fullAdder (high, zip a (repeat high))


alu :: ([Signal Bool], [Signal Bool], Signal Bool, Signal Bool, Signal Bool, Signal Bool, Signal Bool, Signal Bool)
      -> ([Signal Bool], Signal Bool, Signal Bool)
alu (x, y, zx, nx, zy, ny, f, no) = (out', zr, ng)
    where
      zr   = undefined  -- map or2 (map equalBool out' (repeat low))
      ng   = undefined
      out' = ifBool no (map inv out, out)
      out  = ifBool f (rippleCarryAdder x'' y'', andLifted x'' y'')
      x'   = ifBool zx (repeat low, x)
      x''  = ifBool nx (inv x', x')
      y'   = ifBool zy (repeat low, y)
      y''  = ifBool ny (inv y', y')

