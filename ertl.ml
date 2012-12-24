open Rtl
(*
Conventions d'appel: (Avant dernier cours)
-Quatres premiers arguments dans $a0,1,2,3
-Résultat renvoyé dans $v0
-Les registres s sont sauvegardés par l'appelé, les autres par l'appelant.
*)
type instr=
|Ecall of ident*int*label
|Esyscall of label
|Ealloc_frame of label
|Edelete_frame of label
|Eget_stack_param of register*int*label
|Eset_stack_param of register*int*label
|Ereturn
|Emunop of register*munop*register*label
|Embinop of register*mbinop*register*label
|Econst of register*int*label

