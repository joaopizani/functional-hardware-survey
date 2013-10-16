{-# LANGUAGE TemplateHaskell #-}
module TutorialComponents where

import ForSyDe
import Data.Int (Int16)

import Tutorial1 (plus1SysDef)


addFourProc :: Signal Int16 -> Signal Int16
addFourProc = p1c "plus1_1" . p1c "plus1_2" . p1c "plus1_3" . p1c "plus1_4"
    where p1c name = instantiate name plus1SysDef

addFourSysDef :: SysDef (Signal Int16 -> Signal Int16)
addFourSysDef = newSysDef addFourProc "addFour" ["in1"] ["out1"]

addFourSim :: [Int16] -> [Int16]
addFourSim = simulate addFourSysDef

addFourVHDL :: IO ()
addFourVHDL = writeVHDL addFourSysDef

addFourGraph :: IO ()
addFourGraph = writeGraphML addFourSysDef

