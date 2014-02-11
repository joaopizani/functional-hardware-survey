zProc :: ProcId -> S Bit -> S Word -> S Word
zProc name = zipWithSY name $(newProcFun [d|
    f :: Bit -> Word -> Word
    f z w = if z == H then 0 else w |])

nProc :: ProcId -> S Bit -> S Word -> S Word
nProc name = zipWithSY name $(newProcFun [d|
    f :: Bit -> Word -> Word
    f n w = if n == H then negate w else w |])

compProc :: S Bit -> S Word -> S Word -> S Word
compProc = zipWith3SY "compProc" $(newProcFun [d|
    f :: Bit -> Word -> Word -> Word
    f o x y = if o == H then x + y else x .&. y |])

tzProc :: S Word -> S Bit ...
tnProc :: S Word -> S Bit ...
