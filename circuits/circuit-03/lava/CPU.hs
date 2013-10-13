module CPU where

import Lava

import ALU (SB, increment, ALUControlBits)
import Register (regN)


-- | A Hack instruction is a vector of 16 bits
type HackInstruction = (SB, SB, SB, SB, SB, SB, SB, SB, SB, SB, SB, SB, SB, SB, SB, SB)

cpu :: ( [Signal Bool]    -- inM: M value input (M = contents of RAM[A]
       , HackInstruction  -- instruction of Hack assembly
       , Signal Bool)     -- whether to restart current program or just continue
       -> ( [Signal Bool]   -- outM: M value output
         , Signal Bool      -- writeM: whether to write to M
         , [Signal Bool]    -- addressM: address of M in data memory
         , [Signal Bool] )  -- pc: address of the next instruction

cpu (inM, instruction, reset) = (outM, writeM, addressM, pc)
  where
    outM     = undefined
    writeM   = undefined
    addressM = undefined
    pc       = undefined

    (c1,c2,c3,c4,c5,c6,ca) = instructionDecoder instruction


-- | Control bits inside the CPU (nine)
type CPUControlBits = (SB, SB, SB, SB, SB, SB, ALUControlBits)

instructionDecoder :: HackInstruction -> CPUControlBits
instructionDecoder i@(i00,i01,i02,i03,i04,i05,i06,i07,i08,i09,i10,i11,i12,i13,i14,i15)
    = (c1,c2,c3,c4,c5,c6,ca)
  where
    c1 = undefined
    c2 = undefined
    c3 = undefined
    c4 = undefined
    c5 = undefined
    c6 = undefined
    ca = (i04, i05, i06, i07, i08, i09)


-- | The program counter is a straightforward counter, which can be reset and set to a particular
-- value, and counts from this value upwards, and with 0 right after Nmax.
programCounter :: Int -> (SB, SB, [SB]) -> [SB]
programCounter n (reset, set, address) = out
  where
    incr     = increment out
    out      = delay (replicate n low) increset  -- begin with all high to force first zero
    incaddr  = mux (set, (incr, address))
    increset = mux (reset, (incaddr, replicate n low))

testPC1 :: [[SB]]
testPC1 = simulateSeq (programCounter 3) inputs
  where
    inputs =
      [ (low, low, [low, low, low])
      , (low, low, [low, low, low])
      , (low, low, [low, low, low])
      , (low, high, [high, low, low])
      , (low, low, [low, low, low])
      , (low, low, [low, low, low])
      , (high, low, [low, low, low])
      , (low, low, [low, low, low])
      , (low, low, [low, low, low])
      , (low, low, [low, low, low])
      , (low, low, [low, low, low]) ]

