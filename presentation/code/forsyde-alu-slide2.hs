aluFunc :: ProcFunc (ALUCtrl -> Word -> Word -> (Word,ALUFlag))
aluFunc = $(newProcFun [d|
  aluFunc' (zx,nx,zy,ny,f,no) x y =
      ( out,  (bb (out == 0), bb (out < 0)) )
    where
      zf z w   = if bo z then 0 else w
      nf n w   = if bo n then complement w else w
      (xn, yn) = (nf nx $ zf zx $ x,  nf ny $ zf zy $ y)
      out      = nf no $ case f of
                          ALUSum -> xn + yn
                          ALUAnd -> xn .&. yn  |] )

aluProc :: S ALUCtrl -> S Word -> S Word -> S (Word,ALUFlag)
aluProc = zipWith3SY "aluProc" aluFunc
