testPC3 :: Bool
testPC3 = (simulate pcSysDef) resets sets vals == expected
    where
        (r, s)   = (H, H)  -- nicknames for reset and set
        x        = 0       -- nickname for "don't care"
        expected = [0, 1, 2, 3, 1, 2, 3, 0, 1, 2, 3, 4]
        (resets, sets, vals) = unzip3 inputs
        inputs = [ (L,L,x), (L,L,x), (L,L,x), (L,s,1), (L,L,x), (L,L,x)
                 , (r,L,x), (L,L,x), (L,L,x), (L,L,x), (L,L,x), (L,L,x) ]
