newtype Signal a = Signal Symbol
newtype Symbol = Symbol (Ref (S Symbol))

data S s
    = Bool      Bool
    | Inv       s
    | And       [s]
    | Or        [s]
    | Xor       [s]
    | VarBool   String
    | DelayBool s s  -- other constructors...
