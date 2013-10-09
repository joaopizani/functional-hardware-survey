module CPU where

import Lava


-- | Simple abbreviation of Signal Bool
type SB = Signal Bool

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

    (c1,c2,c3,c4,c5,c6,c7,c8,c9) = instructionDecoder instruction


-- | Control bits inside the CPU (nine)
type CPUControlBits = (SB, SB, SB, SB, SB, SB, SB, SB, SB)

instructionDecoder :: HackInstruction -> CPUControlBits
instructionDecoder i@(i00,i01,i02,i03,i04,i05,i06,i07,i08,i09,i10,i11,i12,i13,i14,i15)
    = (c1,c2,c3,c4,c5,c6,c7,c8,c9)
  where
    c1 = undefined
    c2 = undefined
    c3 = undefined
    c4 = undefined
    c5 = undefined
    c6 = undefined
    c7 = undefined
    c8 = undefined
    c9 = undefined

