Ltac tac :=  
    rinvert;                (* destruct the circuit *)
    realise_all;            (* use the hint data-base *)
    unreify_all bool;       (* unreify *)
    destruct_all;           (* destruct the booleans *)
    intros_all;
    clear;
    boolean_eq.
