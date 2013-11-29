Lemma Implement_adder n cin a b cout s :
    Implement (RIPPLE cin a b cout s n)
    ([b:_] & Iso_Phi _ n & Iso_Phi _ n)%reif
    (Iso_Phi _ n & [b:_ ])%reif
    (fun x => match x with (c,a,b) => add n a b c end).

Proof.
    revert cin cout.
    induction n.
    intros cin cout ins outs H. unfold RIPPLE in H.
    realise_all.
    rewrite H. clear. unreify_all bool.
    destruct_all.
    apply eqT_true.
    rewrite (Word.eq_zero w (Word.repr 0 1)).
    rewrite (Word.eq_zero w0 (Word.repr 0 0)).
    destruct b0; reflexivity.

    (* recursive case *)
    intros cin cout ins outs H.
    simpl in H.
    rinvert.
    apply IHn in Hk8.
    apply FADD_2 in Hk4.
    apply (ONE_Implement (I _ _ & I _ _)%reif) in Hk9.
    apply (ONE_Implement (I _ 1)%reif) in Hk2.
    apply (ONE_Implement ([b:_])%reif) in Hk7.
    apply (ONE_Implement ([b:_])%reif) in Hk6.
    clear IHn.
    apply (Implement_combine' s 1 n) in Hk0.
    apply (Implement_high_lows a b 1 n) in Hk10.
    realise_all.

    simpl in outs.
    unreify_all bool.

    destruct_all.
    unfold fst, snd.
    intros_all.
    unfold id.
    change (S n) with (1 + n).
    symmetry.
    rewrite (Word.combine_low_high 1 n w) at 1.
    rewrite (Word.combine_low_high 1 n w0) at 1.
    unfold add.
    rewrite Word.add_add'. unfold Word.add'. reflexivity.
Qed.
