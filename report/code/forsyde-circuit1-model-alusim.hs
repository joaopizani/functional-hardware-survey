type WordType = Int16

data ALUOp = ALUSum | ALUAnd
    deriving (Typeable, Data, Show)

$(deriveLift1 ''ALUOp)

type ALUControl = (Bit, Bit, Bit, Bit, ALUOp, Bit)

type ALUFlags = (Bit, Bit)


bo, bb :: Bit -> Bool
bo = bitToBool
bb = boolToBit

aluFunc :: ProcFun (ALUControl -> WordType -> WordType -> (WordType, ALUFlags))
aluFunc =
    $(newProcFun
        [d|
            aluFunc :: ALUControl -> WordType -> WordType -> (WordType, ALUFlags)
            aluFunc (zx,nx,zy,ny,f,no) x y = (out, (bb (out == 0), bb (out < 0)))
                where
                    zf z w   = if bo z then 0 else w
                    nf n w   = if bo n then complement w else w
                    (xn, yn) = (nf nx $ zf zx $ x,  nf ny $ zf zy $ y)
                    out      = nf no $ case f of
                                           ALUSum -> xn + yn
                                           ALUAnd -> xn .&. yn  |] )

aluProc :: Signal ALUControl -> Signal WordType -> Signal WordType
        -> Signal (WordType, ALUFlags)
aluProc = zipWith3SY "aluProc" aluFunc

aluSysDef :: SysDef (  Signal ALUControl -> Signal WordType -> Signal WordType
                    -> Signal (WordType, ALUFlags) )
aluSysDef = newSysDef aluProc "alu" ["ctrl", "x", "y"] ["outs"]
