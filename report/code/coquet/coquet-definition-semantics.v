Inductive Semantics : forall {n} {m},
    circuit n m -> (n -> Data) -> (m -> Data) -> Prop :=

| KAtom : forall n m {Hfn : Fin n} {Hfm : Fin m}
              (t : techno n m) ins outs,
                  spec t ins outs -> Semantics (Atom t) ins outs

| KSer : forall n m p (x : circuit n m) (y : circuit m p) ins middles outs,
                    Semantics x ins middles
                 -> Semantics y middles outs
                 -> Semantics (Ser x y) ins outs

| KPar : forall n m p q (x : circuit n p) (y : circuit m q) ins outs,
                Semantics x (select_left ins) (select_left outs)
             -> Semantics y (select_right ins) (select_right outs)
             -> Semantics (Par x y) ins outs

| KPlug : forall n m {Hfn : Fin n} {Hfm : Fin m} (f : m -> n) ins,
              Semantics (Plug f) ins (Data.lift f ins)

| KLoop : forall n m l (x : circuit (n + l) (m + l)) ins outs retro,
                 Semantics x (Data.app ins retro) (Data.app outs retro)
              -> Semantics (Loop x) ins outs
