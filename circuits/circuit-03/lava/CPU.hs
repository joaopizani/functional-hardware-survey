module CPU where

import Lava

import ALU (alu, SB, increment, ALUControlBits)
import Register (regN)


-- | The program counter is a straightforward counter, which can be reset and set to a particular
-- value, and counts from this value upwards, and with 0 right after Nmax.
programCounter :: Int -> (SB, SB, [SB]) -> [SB]
programCounter n (reset, set, input) = out
  where
    incr     = increment out
    out      = delay (replicate n low) increset
    incinput = mux (set, (incr, input))
    increset = mux (reset, (incinput, replicate n low))

-- Expected behaviour: counts normally from zero, then is set to one and counts from there.
-- Finally, while in a high count, is reset and starts over from zero.
testPC3 :: [[SB]]
testPC3 = simulateSeq (programCounter wordSize) inputs
  where
    wordSize = 3
    reset    = high
    set      = high
    whatever = replicate wordSize low
    one      = high : replicate (wordSize - 1) low
    inputs =
      [ (low,   low, whatever)
      , (low,   low, whatever)
      , (low,   low, whatever)
      , (low,   set, one)
      , (low,   low, whatever)
      , (low,   low, whatever)
      , (reset, low, whatever)
      , (low,   low, whatever)
      , (low,   low, whatever)
      , (low,   low, whatever)
      , (low,   low, whatever)
      , (low,   low, whatever) ]
{-
    zero  = whatever
    two   = low  : high : replicate (wordSize - 2) low
    three = high : high : replicate (wordSize - 2) low
    four  = low  : low  : high
    outputs = [zero, one, two, three, one, two, three, zero, one, two, three, four]
-}


-- | Bits with conditions on which to jump (out < 0, out = 0, out > 0)
type JumpCondBits = (SB, SB, SB)

-- | Circuit to decide whether to set or not the program counter (PC), given the
-- Jump Condition bits from the instruction and the ZR and NG flags from the ALU
decideSetPC :: (JumpCondBits, SB, SB) -> SB
decideSetPC ((jlt, jeq, jgt), stZR, stNG) = undefined

-- | Control bits affecting CPU behaviour, derived from the instruction itself
type CPUControlBits = (SB, SB, DestBits, JumpCondBits, ALUControlBits)

-- | Bits deciding where to store the result of computation: (A, D, M)
type DestBits = (SB, SB, SB)

instructionDecoder :: HackInstruction -> CPUControlBits
instructionDecoder (i00, _, _, i03, i04, i05, i06, i07, i08, i09, i10, i11, i12, i13, i14, i15)
    = (aFlag, cAM, cDest, cJump, cALU)
  where
    aFlag = i00
    cAM   = inv i03
    cDest = (i10, i11, i12)
    cJump = (i13, i14, i15)
    cALU  = (i04, i05, i06, i07, i08, i09)


-- | A Hack instruction is a vector of 16 bits
type HackInstruction = (SB, SB, SB, SB, SB, SB, SB, SB, SB, SB, SB, SB, SB, SB, SB, SB)

cpu :: Int                  -- The word size of the CPU
    -> ( [Signal Bool]      -- inM: M value input (M = contents of RAM[A]
       , HackInstruction    -- instruction of Hack assembly
       , Signal Bool)       -- whether to restart current program or just continue
       -> ( [Signal Bool]   -- outM: M value output
         , Signal Bool      -- writeM: whether to write to M
         , [Signal Bool]    -- addressM: address of M in data memory
         , [Signal Bool] )  -- pc: address of the next instruction
cpu wordSize (inM, instruction, reset) = (outM, writeM, addressM, pc)
  where
    (i00,i01,i02,i03,i04,i05,i06,i07,i08,i09,i10,i11,i12,i13,i14,i15) = instruction
    insList = [i00,i01,i02,i03,i04,i05,i06,i07,i08,i09,i10,i11,i12,i13,i14,i15]

    outM                 = aluOut
    addressM             = aReg
    aReg                 = regN wordSize (aMux, setA)
    dReg                 = regN wordSize (aluOut, setD)
    aMux                 = mux (aFlag, (insList, aluOut))
    am                   = mux (cAM, (inM, aReg))
    (aluOut, stZR, stNG) = alu (dReg, am, cALU)
    pc                   = programCounter wordSize (reset, setPC, aReg)

    setPC = decideSetPC (cJump, stZR, stNG)
    setA  = or2 (inv aFlag, and2 (aFlag, writeA))
    setD  = and2 (aFlag, writeD)

    (aFlag, cAM, cDest, cJump, cALU) = instructionDecoder instruction
    (writeA, writeD, writeM)         = cDest

