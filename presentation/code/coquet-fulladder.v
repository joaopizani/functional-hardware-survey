Definition HADD a b s c: circuit ([:a]+[:b]) ([:s]+[:c]) :=
    Fork2 ([:a] + [:b])  |>  (XOR a b s  &  AND a b c).


Program Definition FADD a b cin sum cout :
    circuit ([:cin] + ([:a] + [:b])) ([:sum] + [:cout]) :=

   (ONE [: cin]  &  HADD a b "s" "co1")
|> Rewire (*  (a, (b,c)) => ((a,b), c)  *)
|> (HADD cin "s" sum "co2" & ONE [: "co1"])
|> Rewire (*  ((a,b), c) => (a, (b,c))  *)
|> (ONE [:sum]  &  OR "co2" "co1" cout).

Next Obligation. revert H; plug_def. Defined.
Next Obligation. plug_auto. Defined.
Next Obligation. revert H; plug_def. Defined.
Next Obligation. plug_auto.Defined.
