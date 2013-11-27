ram64 :: Signal WordType -> Signal (FSVec D6 Bit) -> Signal Bit
      -> Signal WordType
ram64 input addr load = mux' addr (zipxSY "zipRows" rs)
    where
        -- parts declarations
        mux'     = instantiate "mux" mux64SysDef
        decoder' = instantiate "decoder" decode6To64SysDef
        reg' l   = instantiate l regSysDef
        and' l   = instantiate l andSysDef
        -- using the parts
        r (s, l) = (reg' l) input ((and' (l ++ ":and")) load s)
        rs'      = unzipxSY "unzipAddr" $ decoder' addr
        rs       = V.map r $ V.zip rs' (V.map (\n -> "r" ++ show n)
                                           (V.unsafeVector d64 [0..63]))

ram64SysDef :: SysDef ( Signal WordType -> Signal (FSVec D6 Bit) -> Signal Bit
                     -> Signal WordType)
ram64SysDef = newSysDef ram64 "ram64" ["input","addr","load"] ["outWord"]
