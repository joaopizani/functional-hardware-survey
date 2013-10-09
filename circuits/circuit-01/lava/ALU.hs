module ALU where

import Lava
import Lava.Patterns


-- | Simple abbreviation of Signal Bool
type SB = Signal Bool


halfAdder :: (SB, SB) -> (SB, SB)
halfAdder inputs = (xor2 inputs, and2 inputs)

verifyHalfAdder :: [(SB, SB)]
verifyHalfAdder = simulateSeq halfAdder input
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


fullAdder :: (SB, (SB, SB)) -> (SB, SB)
fullAdder (cin, (a, b)) = (s, cout)
    where
      (ab, c1) = halfAdder (a, b)
      (s, c2)  = halfAdder (ab, cin)
      cout     = or2 (c1, c2)


verifyFullAdder :: [(SB, SB)]
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


rippleCarryAdder :: [(SB, SB)] -> [SB]
rippleCarryAdder ab = s
    where (s, _) = row fullAdder (low, ab)

testRippleCarryAdder :: [[SB]]
testRippleCarryAdder = simulateSeq rippleCarryAdder input
    where
      input = [ [ (low,low),(low,low),(low,low),(low,low),(low,low),(low,low),(low,low),(low,low)
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
testIncrement = simulateSeq increment inputs
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
      out'        = ifThenElse no (out, map inv out)
      zr          = foldl (curry or2) low out'
      ng          = equalBool low (last out')
      out         = let xy'' = zip x'' y'' in mux (f, (andLifted xy'', rippleCarryAdder xy''))
      x'          = ifThenElse zx (x, replicate (length x) low)
      x''         = ifThenElse nx (x', map inv x')
      y'          = ifThenElse zy (y, replicate (length x) low)
      y''         = ifThenElse ny (y', map inv y')

testALU :: [([SB], SB, SB)]
testALU = simulateSeq alu inputs
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
      outputs = [ (low16,                            high, low)
                , (replicate 15 low ++ [high],       low,  low)
                , (replicate 16 high,                low, high)
                , (replicate 16 low,                 high, low)
                , (replicate 16 high,                low, high)
                , (replicate 16 high,                low, high)
                , (replicate 16 low,                 high, low)
                , (replicate 16 low,                 high, low)
                , (replicate 15 low ++ [high],       low, low)
                , (replicate 15 low ++ [high],       low, low)
                , (replicate 16 low,                 high, low)
                , (replicate 16 high,                low, high)
                , (replicate 15 high ++ [low],       low, high)
                , (replicate 16 high,                low, high)
                , (replicate 15 low ++ [high],       low, low)
                , (replicate 16 high,                low, high)
                , (replicate 16 low,                 high, low)
                , (replicate 16 high,                low, high)
                ]
-}

