Context a load out : string
Program Definition REGISTER : Circuit ([:load] + [:a]) [:out] :=
    @Loop _ ([:load] + [:a]) [:out] [:out]
    (
           RewireE (*  (load, a, out) => (a, out, load)  *)
        |> MUX2 a out load "in_dff"%string
        |> DFF "in_dff" out
        |> Fork2 [:out]
    )
Next Obligation. revert H. plug_def. Defined.
Next Obligation. plug_auto. Qed.
