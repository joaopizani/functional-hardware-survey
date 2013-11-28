type WordType = Int16
type ALUOp = Bit
type ALUControl = (Bit, Bit, Bit, Bit, ALUOp, Bit)
type ALUFlags = (Bit, Bit)

zProc :: ProcId -> Signal Bit -> Signal WordType -> Signal WordType
zProc name = zipWithSY name $(newProcFun [d| f :: Bit -> WordType -> WordType
                                             f z w = if z == H then 0 else w |])

nProc :: ProcId -> Signal Bit -> Signal WordType -> Signal WordType
nProc name = zipWithSY name $(newProcFun [d| f :: Bit -> WordType -> WordType
                                             f n w = if n == H then 42 else w |])

compProc :: Signal ALUOp -> Signal WordType -> Signal WordType -> Signal WordType
compProc = zipWith3SY "compProc"
               $(newProcFun [d| f :: ALUOp -> WordType -> WordType -> WordType
                                f o x y = if o == H then x + y else x .&. y |])

tzProc :: Signal WordType -> Signal Bit
tnProc :: Signal WordType -> Signal Bit

aluProc :: Signal ALUControl -> Signal WordType -> Signal WordType
        -> Signal (WordType, ALUFlags)
aluProc c x y = zipSY "aluProc" out (zipSY "flagsProc" (tzProc out) (tnProc out))
    where
        (zx,nx,zy,ny,f,no) = unzip6SY "ctrlProc" c
        out  = nProc "no" no comp
        comp = compProc f (nProc "nx" nx $ zProc "zx" zx $ x)
                          (nProc "ny" ny $ zProc "zy" zy $ y)
