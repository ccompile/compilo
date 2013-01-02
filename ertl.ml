(*
Conventions d'appel: (Avant dernier cours)
-Quatres premiers arguments dans $a0,1,2,3
-Résultat renvoyé dans $v0
-Les registres s sont sauvegardés par l'appelé, les autres par l'appelant.
*)
open Rtl
open Register

type label = int


type instr=
  | Ecall of string*int*label
  | Esyscall of label
  | Ealloc_frame of label
  | Edelete_frame of label
  | Eget_stack_param of register*int*label
  | Eset_stack_param of register*int*label
  | Ereturn
  | Econst of register*int*label
(*Suite ne change pas de précedemment*)
  | Emove of register*register*label
  | ELi   of register * int32 * label
  | ELa   of register * label * label
  | ELw   of register * address * label
  | ESw   of register * address * label
  | EArith of Mips.arith * register * register * operand * label
  | ENeg of register * register* label
(*| Set *)
  | Egoto   of label
  | EBeq  of register * register * label * label
  | EBeqz of register * label * label
  | EBnez of register * label * label
  | EJr   of register
  | EReturn 

module M = Map.Make(struct type t=label
    let compare = compare end)

type graph = instr M.t


let pseudoreg_counter = ref 0

let fresh_pseudoreg () =
    let oldval = !pseudoreg_counter in
    incr pseudoreg_counter;
    Pseudo oldval

let label_counter = ref 1

let fresh_label () =
    let oldval = !label_counter in
    incr label_counter;
    oldval

let max_label () =
    !label_counter

let graph = ref M.empty

let reset_graph () =
    graph := M.empty;
    pseudoreg_counter := 0




let generate instr =
    let lbl = fresh_label () in
    graph := M.add lbl instr !graph;
    lbl

let move src dst l = generate (Emove (src, dst, l))
let set_stack r n l = generate (Eset_stack_param (r, n, l))

let assoc_formals formals =
  let rec assoc = function
     | [],_ -> [], []
     | rl, [] -> [], rl
     | r :: rl, p :: pl ->
         let a, rl = assoc (rl, pl) in (r, p) :: a, rl
  in
  assoc (formals, Register.parameters)


let compil_instr = function
  | Rtl.Call (x, rl,r,l) ->
    let frl, fsl = assoc_formals rl in
    let n = List.length frl in
    let l = generate (Ecall (x, n, move Register.result r l)) in
    let ofs = ref 0 in
    let l = List.fold_left
       (fun l t -> ofs := !ofs - 4; set_stack t !ofs l)
       l fsl
    in
    let l = List.fold_right (fun (t, r) l -> move t r l) frl l in
    Egoto l

  | Rtl.Putchar(r,bidon, l) ->
    Econst (Register.v0, 1, (
    move r Register.a0 (generate (
    Esyscall (generate (
    Econst (Register.a0, 10, generate (
    Econst (Register.v0, 11, generate (
    Esyscall l))))))))))

  | Rtl.Sbrk ( n,r, l) ->
    Emove (n,Register.a0,  generate (
    Econst (Register.v0, 9, generate (
    Esyscall (
    move Register.v0 r l)))))

  | Rtl.Move(a,b,c)->Emove(a,b,c) 
  | Rtl.Li(a,b,c)   ->ELi(a,b,c)
  | Rtl.Lw(a,b,c)    ->ELw(a,b,c)
  | Rtl.Sw(a,b,c)   -> ESw(a,b,c)
  | Rtl.Arith(a,b,c,d,e)->EArith(a,b,c,d,e) (*TODO quel est la sortie?*)
  | Rtl.Neg(a,b,c)  ->ENeg(a,b,c)
  | Rtl.B(a)    ->Egoto(a)
  | Rtl.Beq(a,b,c,d)  ->EBeq(a,b,c,d)
  | Rtl.Beqz(a,b,c) ->EBeqz(a,b,c)
  | Rtl.Bnez(a,b,c)  ->EBnez(a,b,c)
  | Rtl.Return(a ) -> 
        if a = None then EReturn else let Some b=a in  
          Egoto(move b (Register.v0) (generate (EReturn))) 
  | _ -> assert(false)

let entry savers formals entry =
  let frl, fsl = assoc_formals formals in
  let ofs = ref 0 in
  let l = List.fold_left
    (fun l t -> ofs := !ofs - 4; set_stack t !ofs l)
    entry fsl
  in
  let l = List.fold_right (fun (t, r) l -> move r t l) frl l in
  let l = List.fold_right (fun (t, r) l -> move r t l) savers l in
  generate (Ealloc_frame l)

let exit savers retr exitl =
  let l = generate (Edelete_frame (generate Ereturn)) in
  let l = List.fold_right (fun (t, r) l -> move t r l) savers l in
  let l = move retr Register.result l in
  graph := M.add exitl (Egoto l) !graph
(*TODO : DEFFUN
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
 *)
