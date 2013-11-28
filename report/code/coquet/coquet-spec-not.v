Definition NOT x nx : Circuit [:x] [:nx]  :=  Fork2 _ |> (NOR x x nx).

Instance NOT_Implement {x nx} : Implement (NOT x nx) _ _ negb.
Proof.
    intros ins outs H.
    unfold NOT in H.
    tac.
Qed.
