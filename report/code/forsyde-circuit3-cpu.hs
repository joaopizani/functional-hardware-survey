hackCPU :: Signal WordType  -- ^ inM: M value input (M = contents of RAM[A]
        -> HackInstruction  -- ^ instruction of Hack assembly
        -> Signal Bit       -- ^ reset
        -> Signal ( WordType  -- ^ outM: M value output
                 , Bit       -- ^ writeM: whether to write to M
                 , AddrType  -- ^ addressM: address of M in data memory
                 , AddrType  -- ^ pc: address of the next instruction
                 )
hackCPU inM instruction reset = zip4SY "zipOuts" aluOut writeM aReg nextInst
    where
        -- parts declaration
        mux2' l     = instantiate (l ++ ":mux") mux2SysDef
        aReg'       = instantiate "aReg" regSysDef
        dReg'       = instantiate "dReg" regSysDef
        alu'        = instantiate "alu" aluSysDef
        decideJump' = instantiate "decideJump" decideJumpSysDef
        pc'         = instantiate "pc" pcSysDef
        orSetA'     = instantiate "setA:or" orSysDef
        invSetA'    = instantiate "setA:inv" invSysDef
        and' l      = instantiate (l ++ ":and") andSysDef
        decoder'    = instantiate "decoder" decoderSysDef
        -- using the parts
        aReg = aReg' aMux setA
        dReg = dReg' aluOut setD
        aMux = (mux2' "aMux") aFlag instruction aluOut
        am   = (mux2' "am") cAM inM aReg
        nextInst = pc' reset setPC aReg

        (aFlag, cAM, cDest, cJump, cALU) = unzip5SY "unzipDecoder" (decoder' instruction)
        (writeA, writeD, writeM) = unzip3SY "unzipDest" cDest

        (aluOut, aluFlags) = unzipSY "unzipALU" (alu' cALU dReg am)
        setPC = decideJump' cJump aluFlags
        setD = (and' "setD") aFlag writeD
        setA = orSetA' (invSetA' aFlag) ((and' "setA") aFlag writeA)
