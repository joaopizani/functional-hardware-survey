module Chapter3.Chapter3 where

import Lava


adder :: (Signal Bool, ([Signal Bool], [Signal Bool]))
      -> ([Signal Bool], Signal Bool)

adder (ci, ([], [])) = ([], ci)
adder (ci, (a:as, b:bs)) = (sum:sums, co)
    where
      (summ, ca) = fullAdd (ci, (a, b))
      (sums, co) = adder (ca, (as, bs))

