Instance Register_Spec : Realise
    (Rn : Iso ([:load] + [:a] -> stream bool) (stream (bool * bool)))
    (* ... isomorphism on outputs ... *)
    (fun (ins : stream (bool * bool)) (outs : stream bool) =>
      outs = pre false
               (fun t => if fst (ins t) then snd (ins t) else outs t))
