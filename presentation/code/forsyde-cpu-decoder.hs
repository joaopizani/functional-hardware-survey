type HackInstruction = FSVec D16 Bit
type Dest = (Bit, Bit, Bit)
type Jump = (Bit, Bit, Bit)

instructionDecoder :: S HackInstruction
                     -> S (Bit, Bit, Dest, Jump, ALUCtrl)
instructionDecoder = mapSY "mapSYdecoder" decoderFun where
  decoderFun = $(newProcFun [d|
    f :: HackInstruction -> (Bit, Bit, Dest, Jump, ALUCtrl)
    f i = ( i!d0
          , not (i!d3)
          , (i!d10, i!d11, i!d12)
          , (i!d13, i!d14, i!d15)
          , (i!d4,  i!d5,  i!d6, i!d7, i!d8, i!d9)
          ) |])
