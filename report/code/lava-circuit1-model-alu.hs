type ALUControlBits = (SB, SB, SB, SB, SB, SB)

alu :: ([SB], [SB], ALUControlBits) -> ([SB], SB, SB)
alu (x, y, (zx, nx, zy, ny, f, no)) = (out', zr, ng)
    where
        x'   = mux (zx, (x, replicate (length x) low))
        x''  = mux (nx, (x', map inv x'))
        y'   = mux (zy, (y, replicate (length x) low))
        y''  = mux (ny, (y', map inv y'))
        out  = let xy'' = zip x'' y'' in mux (f, (andl xy'', rippleCarryAdder xy''))
        out' = mux (no, (out, map inv out))
        zr   = foldl (curry and2) low out'
        ng   = equalBool high (last out')
