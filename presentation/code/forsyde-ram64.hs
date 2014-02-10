reg :: S Word -> S Bit -> S Word
reg input load = out where
    out = delaySY "delay" (0 :: WordType) dff
    dff = (instantiate "mux2" mux2SysDef) load out input

ram64 :: S Word -> S (FSVec D6 Bit) -> S Bit -> S Word
ram64 input addr load = mux' addr (zipxSY "zipRows" rs) where
  mux'     = instantiate "mux" mux64SysDef
  decoder' = instantiate "decoder" decode6To64SysDef
  reg' l   = instantiate l regSysDef
  and' l   = instantiate l andSysDef
  r (s,l) = (reg' l) input ((and' (l ++ ":and")) load s)
  rs'     = unzipxSY "unzipAddr" $ decoder' addr
  rs      = V.map r $ V.zip rs' (V.map (\n -> "r" ++ show n)
                                 (V.unsafeVector d64 [0..63]))

ram64SysDef = newSysDef ram64 "ram64" ["i","a","l"] ["o"]
