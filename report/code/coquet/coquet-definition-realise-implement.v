Context {n m N M : Type} (Rn : Iso (n -> T) N) (Rm : Iso (m -> T) M).
Class Realise (c : Circuit n m) (R : N -> M -> Prop) :=
    realise :
        forall ins outs, Semantics c ins outs -> R (iso ins) (iso outs)
Class Implement (c : Circuit n m) (f : N -> M) :=
    implement :
        forall ins outs, Semantics c ins outs -> iso outs = f (iso ins)
