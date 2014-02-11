halfAdder :: (Signal Bool, Signal Bool)
            -> (Signal Bool, Signal Bool)
halfAdder inputs = (xor2 inputs, and2 inputs)
