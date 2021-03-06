module ALU where

import Lava
import Lava.Patterns


-- | Simple abbreviation of Signal Bool
type SB = Signal Bool


halfAdder :: (SB, SB) -> (SB, SB)
halfAdder inputs = (xor2 inputs, and2 inputs)

testHalfAdder :: [(SB, SB)]
testHalfAdder = map (simulate halfAdder) input
    where
      input = [ (low,  low)
              , (low,  high)
              , (high, low)
              , (high, high)
              ]
{-
      output = [ (low, low)
               , (high, low)
               , (high, low)
               , (low, high)
               ]
-}

prop_halfAdderNeverBothTrue :: (SB, SB) -> Signal Bool
prop_halfAdderNeverBothTrue inputs = ok
    where
        ok = nand2 (summ, carry)
        (summ, carry) = halfAdder inputs


fullAdder :: (SB, (SB, SB)) -> (SB, SB)
fullAdder (cin, (a, b)) = (s, cout)
    where
      (ab, c1) = halfAdder (a, b)
      (s, c2)  = halfAdder (ab, cin)
      cout     = or2 (c1, c2)

testFullAdder :: [(SB, SB)]
testFullAdder = map (simulate fullAdder) input
    where
      input = [ (low,  (low,  low))
              , (low,  (low,  high))
              , (low,  (high, low))
              , (low,  (high, high))
              , (high, (low,  low))
              , (high, (low,  high))
              , (high, (high, low))
              , (high, (high, high)) ]
{-
      outputs = [ (low, low)
                , (high, low)
                , (high, low)
                , (low, high)
                , (high, low)
                , (low, high)
                , (low, high)
                , (high, high)
                ]
-}

prop_FullAdderCommutative :: (SB, (SB, SB)) -> Signal Bool
prop_FullAdderCommutative (c, (a, b)) = fullAdder (c, (a, b)) <==> fullAdder (c, (b, a))


rippleCarryAdder :: [(SB, SB)] -> [SB]
rippleCarryAdder ab = s
    where (s, _) = row fullAdder (low, ab)

testRippleCarryAdder :: [[SB]]
testRippleCarryAdder = map (simulate rippleCarryAdder) input
    where
      input =
        [ [ (low,low),(low,low),(low,low),(low,low),(low,low),(low,low),(low,low),(low,low)
          , (low,low),(low,low),(low,low),(low,low),(low,low),(low,low),(low,low),(low,low) ]
        , [ (low,high),(low,high),(low,high),(low,high),(low,high),(low,high),(low,high),(low,high)
          , (low,high),(low,high),(low,high),(low,high),(low,high),(low,high),(low,high),(low,high) ]
        ]
{-
      outputs = [replicate 16 low, replicate 16 high]
-}



andLifted :: [(SB, SB)] -> [SB]
andLifted = map and2


increment :: [SB] -> [SB]
increment a = s
    where (s, _) = row fullAdder (high, zip a (replicate (length a) low))

testIncrement :: [[SB]]
testIncrement = map (simulate increment) inputs
    where
      inputs = [ replicate 16 low
               , replicate 16 high
               , (replicate 13 low) ++ [high, low, high]
               , (replicate 13 high) ++ [low, high, high]
               ]
{-
      outputs = [ (replicate 15 low) ++ [high]
                , replicate 16 low
                , (replicate 13 low) ++ [high, high, low]
                , (replicate 14 high) ++ [low, low]
                ]
-}

prop_IncrementIsAlwaysDifferentThanInput_n :: Int -> Property
prop_IncrementIsAlwaysDifferentThanInput_n n = forAll (list n) (\x -> verifyIncrement x)
    where verifyIncrement x = inv (x <==> increment x)


-- | The control bits of the ALU (six)
type ALUControlBits = (SB, SB, SB, SB, SB, SB)

-- Had to group single-bit inputs separately because Lava doesn't provide a Generic
-- instance for tuples with more than 6 elements
-- ASSUMPTION: x and l have the same length
alu :: ( [SB], [SB]        -- numerical inputs
      , ALUControlBits )   -- control bits of the ALU
      -> ([SB], SB, SB)
alu (x, y, (zx, nx, zy, ny, f, no)) = (out', zr, ng)
    where
      x'   = mux (zx, (x, replicate (length x) low))
      x''  = mux (nx, (x', map inv x'))
      y'   = mux (zy, (y, replicate (length x) low))
      y''  = mux (ny, (y', map inv y'))
      out  = let xy'' = zip x'' y'' in mux (f, (andLifted xy'', rippleCarryAdder xy''))
      out' = mux (no, (out, map inv out))
      zr   = foldl (curry and2) low out'
      ng   = equalBool high (last out')

testALU :: [([SB], SB, SB)]
testALU = map (simulate alu) inputs
    where
      low16  = replicate 16 low
      high16 = replicate 16 high
      inputs = [ (low16, high16, (high, low,  high, low,  high, low))
               , (low16, high16, (high, high, high, high, high, high))
               , (low16, high16, (high, high, high, low, high, low))
               , (low16, high16, (low, low, high, high, low, low))
               , (low16, high16, (high, high, low, low, low, low))
               , (low16, high16, (low, low, high, high, low, high))
               , (low16, high16, (high, high, low, low, low, high))
               , (low16, high16, (low, low, high, high, high, high))
               , (low16, high16, (high, high, low, low, high, high))
               , (low16, high16, (low, high, high, high, high, high))
               , (low16, high16, (high, high, low, high, high, high))
               , (low16, high16, (low, low, high, high, high, low))
               , (low16, high16, (high, high, low, low, high, low))
               , (low16, high16, (low, low, low, low, high, low))
               , (low16, high16, (low, high, low, low, high, high))
               , (low16, high16, (low, low, low, high, high, high))
               , (low16, high16, (low, low, low, low, low, low))
               , (low16, high16, (low, high, low, high, low, high))
               ]
{-
      outputs = [ (low16,                     high, low)
                , (high : replicate 15 low,   low,  low)
                , (replicate 16 high,         low, high)
                , (replicate 16 low,          high, low)
                , (replicate 16 high,         low, high)
                , (replicate 16 high,         low, high)
                , (replicate 16 low,          high, low)
                , (replicate 16 low,          high, low)
                , (high : replicate 15 low,   low, low)
                , (high : replicate 15 low,   low, low)
                , (replicate 16 low,          high, low)
                , (replicate 16 high,         low, high)
                , (low : replicate 15 high,   low, high)
                , (replicate 16 high,         low, high)
                , (high : replicate 15 low,   low, low)
                , (replicate 16 high,         low, high)
                , (replicate 16 low,          high, low)
                , (replicate 16 high,         low, high)
                ]
-}

