(*
Conventions d'appel: (Avant dernier cours)
-Quatres premiers arguments dans $a0,1,2,3
-Résultat renvoyé dans $v0
-Les registres s sont sauvegardés par l'appelé, les autres par l'appelant.
*)
open Rtl
open Register

type label = int | Notlabel

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

let assoc formals formals =
  let rec assoc = function
     | [], -> [], []
     | rl, [] -> [], rl
     | r :: rl, p :: pl ->
         let a, rl = assoc (rl, pl) in (r, p) :: a, rl
  in
  assoc (formals, Register.parameters)


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

  | Rtl.Move(a,b,c)->Emove(a,b,c) 
  | Rtl.Li(a,b,c)   ->Eli(a,b,c)
  | Rtl.La(a,b,c)   ->Ela(a,b,c)
  | Rtl.Lw(a,b,c)    ->Elw(a,b,c)
  | Rtl.Sw(a,b,c)   -> ESw(a,b,c)
  | Rtl.Arith(a,b,c,d,e)->EArith(a,b,c,d,e) (*TODO quel est la sortie?*)
  | Rtl.Neg(a,b,c)  ->ENeg(a,b,c)
  | Rtl.B(a)    ->Egoto(a)
  | Rtl.Beq((a,b,c,d)  ->EBeq(a,b,c,d)
  | Rtl.Beqz(a,b,c) ->EBeqz(a,b,c)
  | Rtl.Bnez(a,b,c)  ->EBnez(a,b,c)
  | Rtl.Return(a option) -> begin
        match a with 
          | None-> Notlabel
          | Some b->Emove b (Register.v0) Notlabel 

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
(*TODO : DEFFUN*)
let deffun f =
  graph := (*TODO...on traduit chaque instruction...*)
  let savers =
     List.map (fun r -> Register.fresh(), r)
     (Register.ra :: Register.callee saved)
  in
  let entry =
     fun entry savers f.Rtltree.fun formals f.Rtltree.fun entry
  in
  fun exit savers f.Rtltree.fun result f.Rtltree.fun exit;
  { fun name = f.Rtltree.fun name;
     ...
     fun body = !graph; }
 
