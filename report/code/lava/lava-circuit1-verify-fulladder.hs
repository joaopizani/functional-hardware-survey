prop_FullAdderCommutative :: (SB, (SB, SB)) -> Signal Bool
prop_FullAdderCommutative (c, (a, b)) =
    fullAdder (c, (a, b)) <==> fullAdder (c, (b, a))
