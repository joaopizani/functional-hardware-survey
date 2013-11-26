bool :: Bool -> Signal Bool
bool b = lift0 (Bool b)

low, high :: Signal Bool
low  = bool False
high = bool True

inv :: Signal Bool -> Signal Bool
inv = lift1 Inv

andl, orl, xorl :: [Signal Bool] -> Signal Bool
andl = liftl And
orl  = liftl Or
xorl = liftl Xor

and2 (x, y) = andl [x, y]
or2  (x, y) = orl  [x, y]
xor2 (x, y) = xorl [x, y]

nand2 = inv . and2
nor2  = inv . or2
xnor2 = inv . xor2
