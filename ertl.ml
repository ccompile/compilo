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
  | Einit_addr of register*int*label (* TODO : documenter cette instruction *)
  | Emove of register*register*label
  | ELi   of register * int32 * label
  | ELa   of register * address * label
  | ELw   of register * address * label
  | ESw   of register * address * label
  | ELb   of register * address * label
  | ESb   of register * address * label
  | EAddress of register * register * label
  | EArith of Mips.arith * register * register * operand * label
  | ESet of Mips.condition * register* register* operand* label
  | ENeg of register * register* label
  | Egoto   of label
  | EBeq  of register * register * label * label
  | EBeqz of register * label * label
  | EBnez of register * label * label
  | EJr   of register
  | ELoop_begin of label
  | ELoop_end of label
  | EReturn  

module M = Map.Make(struct type t=label
  let compare = compare end)

type graph = instr M.t

let graph = ref M.empty
let addr_loaded = ref Rset.empty

(* Fonctions de génération du code ERTL *)

let reset_graph () =
  su_offset := Rmap.empty;
  addr_loaded := Rset.empty;
  graph := M.empty

let generate instr =
  let lbl = fresh_label () in
  graph := M.add lbl instr !graph;
  lbl

let add_instr lbl instr =
  graph := M.add lbl instr !graph

let find_instr g lbl =
  M.find lbl g

let iter_instr g fct =
  M.iter fct g

type decl=
  { name :string;
    nb_args : int;
    g : graph;
    entry : label ;
    su_size : int }


let move src dst l = generate (Emove (src, dst, l))
let set_stack r n l = generate (Eset_stack_param (r, n, l))
let get_stack r n l = generate (Eget_stack_param (r,n,l))

let assoc_formals formals =
  let rec assoc = function
    | [],_ -> [], []
    | rl, [] -> [], rl
    | r :: rl, p :: pl ->
      let a, rl = assoc (rl, pl) in (r, p) :: a, rl
  in
  assoc (formals, Register.parameters)


let compil_instr = function
  | Call (x, rl,r,l) ->
    let frl, fsl = assoc_formals rl in
    (* TODO EN CHANTIER *)
    (* et si on sauvegardait plutôt les variables locales ? *)
  (*  let savers = List.map (fun r -> fresh_pseudoreg (), r)
      (Register.caller_saved) in *)
    let n = List.length frl in
  (*  (* restauration des caller-saved *)
    let l = List.fold_right (fun (t, r) l -> move t r l) savers l in *)
    (* appel *)
    let l = generate (Ecall (x, n, move Register.result r l)) in
  (*  (* sauvegarde des caller-saved *)
    let l = List.fold_right (fun (t, r) l -> move r t l) savers l in *)
    (* calcul des arguments *)
    let ofs = ref (-1) in
    let l = List.fold_left
      (fun l t -> ofs := !ofs + 1; set_stack t !ofs l)
      l ( fsl)
    in
    let l = List.fold_right (fun (t, r) l -> move t r l) frl l in
    Egoto l

  | Putchar(r,bidon, l) ->
    ELi(Register.v0,(Int32.of_int 11),
      move r Register.a0 (generate (
        Esyscall (l))))

  | Sbrk ( n,r, l) ->
    Emove (n,Register.a0,  generate (
      ELi (Register.v0, (Int32.of_int 9), generate (
        Esyscall (
          move Register.v0 r l)))))

  | Move(a,b,c)->Emove(a,b,c) 
  | Li(a,b,c)  ->ELi(a,b,c)
  | La(a,b,c)  -> ELa(a,b,c)
  | Lw(a,b,c)  ->ELw(a,b,c)
  | Sw(a,b,c)  -> ESw(a,b,c)
  | Lb(a,b,c)  -> ELb(a,b,c)
  | Sb(a,b,c)  -> ESb(a,b,c)
  | Address(a,b,c) -> EAddress(a,b,c)
  | Arith(a,b,c,d,e)->EArith(a,b,c,d,e) 
  | Set(a,b,c,d,e)->ESet(a,b,c,d,e)
  | Neg(a,b,c)  ->ENeg(a,b,c)
  | B(a)    ->Egoto(a)
  | Beq(a,b,c,d)  ->EBeq(a,b,c,d)
  | Beqz(a,b,c) ->EBeqz(a,b,c)
  | Bnez(a,b,c)  ->EBnez(a,b,c)
  | Loop_begin(l) -> ELoop_begin(l)
  | Loop_end(l)   -> ELoop_end(l)
  | Return(a,exit_label) -> Egoto exit_label

let move_bytes typ =
  generic_move_bytes
    generate
    (fun (a,b,c) -> ELb(a,b,c))
    (fun (a,b,c) -> ESb(a,b,c))
    (fun (a,b,c) -> ELw(a,b,c))
    (fun (a,b,c) -> ESw(a,b,c))
    (fun (a,b,c) -> ELa(a,b,c))
    typ
(*Initialisation d'une fonction*)
let fun_entry savers formals entry su =
  let lbl = ref entry in
  Rmap.iter
    (fun reg (offset,typ) ->
      if not (List.mem reg formals) then
        lbl := generate (Einit_addr(reg,offset,!lbl))
      else
        begin
          let pr = fresh_pseudoreg () in
          lbl := move_bytes typ pr (Areg(Int32.zero,reg)) !lbl;
          lbl := generate (Einit_addr(reg,offset,!lbl));
          lbl := generate (Emove(reg,pr,!lbl))
      end)
    su;
  let frl, fsl = assoc_formals formals in
  let ofs = ref (-1) in
  (* la multiplication par 4 est faite dans LTL *)
  let l = List.fold_left
    (fun l t -> ofs := !ofs + 1; get_stack t !ofs l)
    !lbl ( fsl)
  in
  let l = List.fold_right (fun (t, r) l -> move r t l) frl l in
  let l = List.fold_right (fun (t, r) l -> move r t l) savers l in
  generate (Ealloc_frame l)

(*Sortie d'une fonction*)
let fun_exit savers retr exitl =
  let l = generate (Edelete_frame (generate EReturn)) in
  let l = List.fold_right (fun (t, r) l -> move t r l) savers l in
  let l = move retr Register.result l in
  graph := M.add exitl (Egoto l) !graph

(*Fonction principale de traduction du graphe*)
let mmap g=
  Rtl.M.iter (fun x y -> let a = compil_instr y in graph:= M.add x a (!graph)) g

(*Traduction d'une fonction : corp, entrée et sortie*)
let deffun d =
  reset_graph(); 
  mmap d.Rtl.g;
  let savers =
    List.map (fun r -> fresh_pseudoreg (), r)
      (Register.ra :: Register.callee_saved)
  in
  let entry =
    fun_entry savers d.Rtl.args d.Rtl.entry d.Rtl.su_offset 
  in
  fun_exit savers d.retval d.Rtl.exit;
  { name = d.Rtl.name;
    nb_args = List.length d.Rtl.args;
    g = !graph;
    entry = entry;
    su_size = d.Rtl.su_size }
 

let compile_fichier fichier =
  let rec compile_liste = function
    |[]->[]
    |a::q->let suiv = deffun a in suiv::(compile_liste q) 
  in
  compile_liste fichier

let successeurs = function
  | Emove(_,_,l)
  | ELi(_,_,l)
  | ELa(_,_,l)
  | ELw(_,_,l)
  | ESw(_,_,l)
  | ELb(_,_,l)
  | ESb(_,_,l)
  | EAddress(_,_,l)
  | Einit_addr(_,_,l)
  | EArith(_,_,_,_,l)
  | ESet(_,_,_,_,l)
  | ENeg (_,_,l)
  | Egoto l
  | Esyscall l
  | Ealloc_frame l
  | Edelete_frame l
  | Eget_stack_param(_,_,l)
  | Eset_stack_param(_,_,l)
  | ELoop_begin l
  | ELoop_end l
  | Ecall (_,_,l) -> [l]
  | EBeqz (_,l1,l2)
  | EBnez (_,l1,l2)
  | EBeq (_,_,l1,l2) -> [l1;l2]
  | EReturn
  | EJr _ -> []

