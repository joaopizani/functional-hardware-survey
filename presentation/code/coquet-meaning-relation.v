Inductive Sem : forall {n} {m},
   C n m -> (n -> Data) -> (m -> Data) -> Prop :=

| KAtom: forall n m {Hfn: Fin n} {Hfm: Fin m}
          (t: techno n m) i o, spec t i o -> Sem (Atom t) i o

| KSer: forall n m p (x: C n m) (y: C m p) i mid o,
         Sem x i mid -> Sem y mid o -> Sem (Ser x y) i o

| KPar: forall n m p q (x: C n p) (y: C m q) i o,
            Sem x (select_left i) (select_left o)
         -> Sem y (select_right i) (select_right o)
         -> Sem (Par x y) i o

| KPlug: forall n m {Hfn: Fin n} {Hfm: Fin m} (f: m -> n) i,
          Sem (Plug f) i (Data.lift f i)

| KLoop: forall n m l (x: C (n + l) (m + l)) i o ret,
             Sem x (Data.app i ret) (Data.app o ret)
          -> Sem (Loop x) i o
