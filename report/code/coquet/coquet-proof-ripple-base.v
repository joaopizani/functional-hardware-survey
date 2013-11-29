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
