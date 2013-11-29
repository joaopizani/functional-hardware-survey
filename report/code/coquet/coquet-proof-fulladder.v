Instance FADD_Implement {a b cin sum cout} :
    Implement (FADD a b cin sum cout) _ _
    (fun x =>
        match x with
        | (c, (a,b)) =>
              (xorb a (xorb b c), (a && b) || c && (xorb a b))%bool
        end).
Proof.
    unfold FADD; intros ins outs H; tac.
Qed.
