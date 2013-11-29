Program Fixpoint RIPPLE cin a b cout s n :
    circuit ([:cin] + sumn [:a] n + sumn [:b] n) (sumn [:s] n + [:cout]) :=
match n with
    | O   => RewireE (*  (c, x, y) => (x, c)  *)
    | S p =>
           RewireE (*  (a, b, c) => (a, (b, c))  *)
        |> (ONE [:cin]  &  high_lows a b 1 p)

        |> RewireE (*  (c, ((a1,b1), (ap,bp))) => (c, a1, b1, (ap,bp))  *)
        |> (FADD' a b cin s "mid"  &  ONE (sumn [:a] p + sumn [:b] p))

        |> RewireE (*  (s, c, (a,b)) => (s, (c,a,b))  *)
        |> (ONE (sumn [:s] 1)  &  RIPPLE ("mid")%string a b cout s p)

        |> RewireE (*  (s1, (s2,c)) => (s1, s2, c)  *)
        |> combine' s 1 p  &  ONE [:cout]
end.
Next Obligation.
    revert H; intros [[]| H]; repeat left; constructor.
Defined.
Next Obligation. abstract plug_auto. Defined.
Next Obligation. abstract plug_auto. Defined.
Next Obligation. revert X; plug_def. Defined.
Next Obligation. abstract plug_auto. Defined.
Next Obligation. revert X; plug_def. Defined.
Next Obligation. abstract plug_auto. Defined.
Next Obligation. abstract plug_auto. Defined.
