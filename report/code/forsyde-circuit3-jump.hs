type JumpType = (Bit, Bit, Bit)

decideJump :: Signal JumpType -> Signal ALUFlags -> Signal Bit
decideJump = zipWithSY "zipWithDecide" decideFun
  where
    decideFun =
        $(newProcFun [d| f :: (Bit, Bit, Bit) -> ALUFlags -> Bit
                         f (jl,je,jg) (stZ,stN) = if stN == H then jl
                                                  else if stZ == H then je
                                                  else jg |])

decideJumpSysDef :: SysDef (Signal JumpType -> Signal ALUFlags -> Signal Bit)
decideJumpSysDef = newSysDef decideJump "decideSys" ["cond", "flags"] ["jump"]
