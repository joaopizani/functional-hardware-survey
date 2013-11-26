type DestBits = (SB, SB, SB)

type JumpCondBits = (SB, SB, SB)

type CPUControlBits = (SB, SB, DestBits, JumpCondBits, ALUControlBits)

instructionDecoder :: HackInstruction -> CPUControlBits
instructionDecoder (i0,_,_,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15)
        = (aFlag, cAM, cDest, cJump, cALU)
    where
        aFlag = i0
        cAM   = inv i3
        cDest = (i10, i11, i12)
        cJump = (i13, i14, i15)
        cALU  = (i4, i5, i6, i7, i8, i9)
