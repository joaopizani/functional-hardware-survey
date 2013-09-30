module Chapter3.Chapter3 where

import Lava

import Chapter2.Chapter2


adder :: (Signal Bool, ([Signal Bool], [Signal Bool]))
      -> ([Signal Bool], Signal Bool)

adder (ci, ([], []))     = ([], ci)
adder (ci, (a:as, b:bs)) = (summ:sums, co)
    where
      (summ, ca) = fullAdd (ci, (a, b))
      (sums, co) = adder (ca, (as, bs))

vhdlAdder = writeVhdlInputOutput "adder" adder ins outs
    where
      ins  = (var "cin", (varList 4 "a", varList 4 "b"))
      outs = (varList 4 "sum", var "cout")


-- Connection patterns
serial circ1 circ2 a = c
    where
      b = circ1 a
      c = circ2 b

row circ (i, [])   = ([], i)
row circ (i, a:as) = (r:rs, o)
    where
      (r, m)  = circ (i, a)
      (rs, o) = row circ (m, as)


bitAdder' = row halfAdd

adder' = row fullAdd


-- Arithmetic
numBreak num = (bit, num_)
    where
      num_  = idiv (num, 2)
      bit   = int2bit digit
      digit = imod (num, 2)

int2bin 0 num = []
int2bin n num = (b:bs)
    where
      (b, num_) = numBreak num
      bs        = int2bin (n-1) num_


-- Exercises
bitSubber :: (Signal Bool, [Signal Bool]) -> [Signal Bool]
bitSubber (a, [])         = []
bitSubber (a, b:bs) =
    if a == low then b:bs
    else
        if b == high then low : bitSubber (low, bs)
        else low : bitSubber (high, bs)


adder2 inps = ss
    where (ss, _) = row fullAdd (low, inps)


-- 3.3: adder takes as input a pair of lists, while adder' takes a list of pairs.


bin2int :: [Signal Bool] -> Signal Int
bin2int = bin2int_ 1
    where
      bin2int_ _ []     = 0
      bin2int_ k (b:bs) = (bit2int b * k) + bin2int_ (k * 2) bs


zipp ([], _)      = []
zipp (a:as, [])   = []
zipp (a:as, b:bs) = (a, b) : zipp (as, bs)

unzipp []         = ([], [])
unzipp ((a,b):xs) = let (ls, rs) = unzipp xs  in  (a : ls, b : rs)


par :: (Signal Bool -> Signal Bool) -> (Signal Bool -> Signal Bool)
    -> ((Signal Bool, Signal Bool) -> (Signal Bool, Signal Bool))
par c1 c2 (a, b) = (d, e)
    where
      d = c1 a
      e = c2 b


multiplier :: ([Signal Bool], [Signal Bool]) -> [Signal Bool]
multiplier ([], _) = []
multiplier (_, []) = []
multiplier (a:as, b:bs) = undefined
