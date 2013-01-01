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

 


let move src dst l = generate (Emunop (dst, Mmove, src, l))
let set stack r n l = generate (Eset stack param (r, n, l))

let compil_instr = function
  | Rtl.call (r, x, rl, l) ->
    let frl, fsl = assoc formals rl in
    let n = List.length frl in
    let l = generate (Ecall (x, n, move Register.result r l)) in
    let ofs = ref 0 in
    let l = List.fold left
       (fun l t -> ofs := !ofs - 4; set stack t !ofs l)
       l fsl
    in
    let l = List.fold right (fun (t, r) l -> move t r l) frl l in
    Egoto l
  | Rtl.Putchar(r,bidon, l) ->
    Econst (Register.v0, 1, (
    move r Register.a0 (generate (
    Esyscall (generate (
    Econst (Register.a0, 10, generate (
    Econst (Register.v0, 11, generate (
    Esyscall l))))))))))
  | Rtl.sbrk (r, n, l) ->
    Econst (Register.a0, n, generate (
    Econst (Register.v0, 9, generate (
    Esyscall (
    move Register.v0 r l)))))


let fun entry savers formals entry =
  let frl, fsl = assoc formals formals in
  let ofs = ref 0 in
  let l = List.fold left
    (fun l t -> ofs := !ofs - word size; get stack t !ofs l)
    entry fsl
  in
  let l = List.fold right (fun (t, r) l -> move r t l) frl l in
  let l = List.fold right (fun (t, r) l -> move r t l) savers l in
  generate (Ealloc frame l)

let fun exit savers retr exitl =
  let l = generate (Edelete frame (generate Ereturn)) in
  let l = List.fold right (fun (t, r) l -> move t r l) savers l in
  let l = move retr Register.result l in
  graph := Label.M.add exitl (Egoto l) !graph
 
