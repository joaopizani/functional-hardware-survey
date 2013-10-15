{-# LANGUAGE TemplateHaskell #-}
module TutorialCounter where

import ForSyDe
import Data.Int (Int16)
import Tutorial1 (addOnef)


counterProc :: Signal Int16
counterProc = out'
  where
    out  = mapSY "addOneProc" addOnef out'
    out' = delaySY "counterProc" 0 out

counterProcSimpl :: Signal Int16
counterProcSimpl = sourceSY "counterProcSimpl" addOnef 0

counterSysDef :: SysDef (Signal Int16)
counterSysDef = newSysDef counterProc "counter" [] ["count"]

counterSimplSysDef :: SysDef (Signal Int16)
counterSimplSysDef = newSysDef counterProcSimpl "counterSimpl" [] ["count"]

counterSim :: [Int16]
counterSim = simulate counterSysDef

counterSimplSim :: [Int16]
counterSimplSim = simulate counterSimplSysDef

