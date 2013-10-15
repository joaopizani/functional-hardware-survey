{-# LANGUAGE TemplateHaskell #-}
module Tutorial1 where

import ForSyDe
import Data.Int (Int16)


addOnef :: ProcFun (Int16 -> Int16)
addOnef = $(newProcFun [d| addOnef :: Int16 -> Int16
                           addOnef n = n + 1 |])

plus1Proc :: Signal Int16 -> Signal Int16
plus1Proc = mapSY "plus1Proc" addOnef

plus1SysDef :: SysDef (Signal Int16 -> Signal Int16)
plus1SysDef = newSysDef plus1Proc "plus1" ["inSig"] ["outSig"]

plus1Sim :: [Int16] -> [Int16]
plus1Sim = simulate plus1SysDef


combLoopSysDef :: SysDef (Signal Int16)
combLoopSysDef = newSysDef s "combLoop" [] ["outSig"]
  where
    s  = mapSY "addOne1" addOnef s'
    s' = mapSY "addOne2" addOnef s

combLoopSim :: [Int16]
combLoopSim = simulate combLoopSysDef

