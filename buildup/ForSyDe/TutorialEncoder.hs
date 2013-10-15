{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module TutorialEncoder where

import ForSyDe
import Language.Haskell.TH.Lift (deriveLift1)

import Data.Generics (Data,Typeable)
import Data.Param.FSVec
import Data.TypeLevel.Num hiding ((==))

data Direction = DLeft | DDown | DRight | DUp
    deriving (Typeable, Data, Show)

$(deriveLift1 ''Direction)

encoderFun :: ProcFun (FSVec D4 Bit -> AbstExt Direction)
encoderFun =
    $(newProcFun
        [d|
            encode :: FSVec D4 Bit -> AbstExt Direction
            encode v = if v ! d0 == H then Prst DLeft  else
                       if v ! d1 == H then Prst DDown  else
                       if v ! d2 == H then Prst DRight else
                       if v ! d3 == H then Prst DUp    else
                       Abst
        |]
    )

encoderProc :: Signal (FSVec D4 Bit) -> Signal (AbstExt Direction)
encoderProc = mapSY "encoderProc" encoderFun

encoderSysDef :: SysDef (Signal (FSVec D4 Bit) -> Signal (AbstExt Direction))
encoderSysDef = newSysDef encoderProc "encoder" ["arrows"] ["direction"]

encoderSim :: [FSVec D4 Bit] -> [AbstExt Direction]
encoderSim = simulate encoderSysDef

