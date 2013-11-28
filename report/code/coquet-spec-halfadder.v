Definition HADD a b s c : Circuit ([:a] + [:b]) ([:s] + [:c]) :=
    Fork2 ([:a] + [:b]) |> (XOR a b s & AND a b c).

Instance HADD_Implement {a b s c} :
    Implement (HADD a b s c) _ _
    (fun (x:bool*bool) => match x with (a,b) => (xorb a b, andb a b) end).
    Proof.
        unfold HADD;  intros ins outs H;  tac.
    Qed.
