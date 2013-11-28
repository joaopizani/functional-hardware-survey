prop_IncrementIsAlwaysDifferentThanInput :: Int -> Property
prop_IncrementIsAlwaysDifferentThanInput n =
        forAll (list n) (\x -> verifyIncrement x)
    where verifyIncrement x = inv (x <==> increment x)