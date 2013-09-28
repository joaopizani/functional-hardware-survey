module Chapter2.Chapter2 where

import Lava

halfAdd (a, b) = (summ, carry)
    where
      summ  = xor2 (a, b)
      carry = and2 (a, b)

fullAdd (ci, (a, b)) = (summ, co)
    where
      (sum1, carry1) = halfAdd (a, b)
      (summ, carry2) = halfAdd (sum1, ci)
      co             = xor2 (carry1, carry2)

test1 = simulate halfAdd (low, low)
test2 = simulate fullAdd (low, (high, low))
test3 = simulate fullAdd (high, (low, high))


swap (a, b) = (b, a)
testSwap = simulateSeq swap [(low, high), (low, low), (high, low)]


copy a = (a, a)
testCopy = simulateSeq copy [low, high]


sort2 :: (Signal Bool, Signal Bool) -> (Signal Bool, Signal Bool)
sort2 (i1, i2) = (o1, o2)
    where
      o1 = and2 (i1, i2)
      o2 = or2 (i1, i2)

testSort = simulateSeq sort2 domain


myVdd :: () -> Signal Bool
myVdd () = high


mux2 :: (Signal Bool, (Signal Bool, Signal Bool)) -> Signal Bool
mux2 (t, (a, b)) = or2 (and2 (a, inv t), and2 (b, t))

testMux2 = simulateSeq mux2 domain


adder3 ((a1, a2, a3), (b1, b2, b3)) = ((s1, s2, s3), co)
    where
      (s1, c1) = fullAdd (low, (a1, b1))
      (s2, c2) = fullAdd (c1,  (a2, b2))
      (s3, co) = fullAdd (c2,  (a3, b3))


