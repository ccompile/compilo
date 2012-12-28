(*
Conventions d'appel: (Avant dernier cours)
-Quatres premiers arguments dans $a0,1,2,3
-Résultat renvoyé dans $v0
-Les registres s sont sauvegardés par l'appelé, les autres par l'appelant.
*)

type label

type pseudoreg=
  | Notreg
  | V0
  | A0
  | Pseudo of int

type address =
  | Alab of string
  | Areg of int * pseudoreg

type operand =
  | Oimm of int
  | Oreg of pseudoreg

type instr=
  | Ecall of ident*int*label
  | Esyscall of label
  | Ealloc_frame of label
  | Edelete_frame of label
  | Eget_stack_param of register*int*label
  | Eset_stack_param of register*int*label
  | Ereturn
  | Emunop of register*munop*register*label
  | Embinop of register*mbinop*register*label
  | Econst of register*int*label
(*Suite ne change pas de précedemment*)
  | Emove of pseudoreg*pseudoreg*label
  | Li   of pseudoreg * int32 * label
  | La   of pseudoreg * label * label
  | Lw   of pseudoreg * address * label
  | Sw   of pseudoreg * address * label
  | Arith of Mips.arith * pseudoreg * pseudoreg * operand * label
  | Neg  of pseudoreg * pseudoreg * label
(*| Set *)
  | B    of label
  | Beq  of pseudoreg * pseudoreg * label
  | Beqz of pseudoreg * label
  | Bnez of pseudoreg * label
  | Jr   of pseudoreg
  | Syscall of label

 
type graph


