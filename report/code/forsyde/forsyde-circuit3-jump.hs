decideJump :: Signal JumpType -> Signal ALUFlags -> Signal Bit
decideJump = zipWithSY "zipWithDecide" decideFun
  where
    decideFun =
        $(newProcFun [d| f :: (Bit, Bit, Bit) -> ALUFlags -> Bit
                         f (jl,je,jg) (stZ,stN) = if stN == H then jl
                                                  else if stZ == H then je
                                                  else jg |])
