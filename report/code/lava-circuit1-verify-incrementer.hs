prop_IncrementIsAlwaysDifferentThanInput :: Int -> Property
prop_IncrementIsAlwaysDifferentThanInput n =
        forAll (list n) (\x -> prop x)
    where prop x = inv (x <==> increment x)
