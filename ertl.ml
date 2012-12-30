(*
Conventions d'appel: (Avant dernier cours)
-Quatres premiers arguments dans $a0,1,2,3
-Résultat renvoyé dans $v0
-Les registres s sont sauvegardés par l'appelé, les autres par l'appelant.
*)
open Rtl

type label = int

type pseudoreg=
  | Notreg
  | V0
  | A0
  | Pseudo of int


type instr=
  | Ecall of ident*int*label
  | Esyscall of label
  | Ealloc_frame of label
  | Edelete_frame of label
  | Eget_stack_param of pseudoreg*int*label
  | Eset_stack_param of pseudoreg*int*label
  | Ereturn
  | Econst of pseudoreg*int*label
(*Suite ne change pas de précedemment*)
  | Emove of pseudoreg*pseudoreg*label
  | ELi   of pseudoreg * int32 * label
  | ELa   of pseudoreg * label * label
  | ELw   of pseudoreg * address * label
  | ESw   of pseudoreg * address * label
  | EArith of Mips.arith * pseudoreg * pseudoreg * operand * label
  | ENeg  of pseudoreg * pseudoreg * label
(*| Set *)
  | Egoto   of label
  | EBeq  of pseudoreg * pseudoreg * label
  | EBeqz of pseudoreg * label
  | EBnez of pseudoreg * label
  | EJr   of pseudoreg

module M = Map.Make(struct type t=label
      let compare=compare end)
 
type graph= instr M.t
let move src dst l = generate (Emunop (dst, Mmove, src, l))
let set stack r n l = generate (Eset stack param (r, n, l))

let compil_instr = function
  | Rtl.call (r, x, rl, l) ->
    let frl, fsl = assoc formals rl in
    let n = List.length frl in
    let l = generate (Ecall (x, n, move Register.result r l)) in
    let ofs = ref 0 in
    let l = List.fold left
       (fun l t -> ofs := !ofs - word size; set stack t !ofs l)
       l fsl
    in
    let l = List.fold right (fun (t, r) l -> move t r l) frl l in
    Egoto l
 
