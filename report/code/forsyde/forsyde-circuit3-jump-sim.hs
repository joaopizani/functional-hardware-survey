testDecideJump :: Bool
testDecideJump = (simulate decideJumpSysDef) conds flags == expected
    where
        (conds, flags) = unzip inputs
        inputs = [ ((L,L,L),(L,L)), ((L,L,L),(L,H)), ((L,L,L),(H,L)), ((L,L,L),(H,H))
                 , ((L,L,H),(L,L)), ((L,L,H),(L,H)), ((L,L,H),(H,L)), ((L,L,H),(H,H))
                 , ((L,H,L),(L,L)), ((L,H,L),(L,H)), ((L,H,L),(H,L)), ((L,H,L),(H,H))
                 , ((L,H,H),(L,L)), ((L,H,H),(L,H)), ((L,H,H),(H,L)), ((L,H,H),(H,H))
                 , ((H,L,L),(L,L)), ((H,L,L),(L,H)), ((H,L,L),(H,L)), ((H,L,L),(H,H))
                 , ((H,L,H),(L,L)), ((H,L,H),(L,H)), ((H,L,H),(H,L)), ((H,L,H),(H,H))
                 , ((H,H,L),(L,L)), ((H,H,L),(L,H)), ((H,H,L),(H,L)), ((H,H,L),(H,H))
                 , ((H,H,H),(L,L)), ((H,H,H),(L,H)), ((H,H,H),(H,L)), ((H,H,H),(H,H)) ]
        expected = [ L, L, L, L, H, L, L, L, L, L, H, L, H, L, H, L
                   , L, H, L, H, H, H, L, H, L, H, H, H, H, H, H, H ]
