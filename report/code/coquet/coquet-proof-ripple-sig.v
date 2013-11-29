Lemma Implement_adder n cin a b cout s :
    Implement (RIPPLE cin a b cout s n)
    ([b:_] & Iso_Phi _ n & Iso_Phi _ n)%reif
    (Iso_Phi _ n & [b:_ ])%reif
    (fun x => match x with (c,a,b) => add n a b c end).
