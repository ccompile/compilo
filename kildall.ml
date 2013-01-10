open Register
open Ertl

(* Fonctions de calcul des use / def *)

let rec prefix n = function
  | [] -> []
  | _ when n = 0 -> []
  | h::t -> h::(prefix (n-1) t)

let list_of_address = function
  | Alab(_) -> []
  | Areg(_,r) -> [r]

let use_def = function 
  | Ecall (_,n,_) -> (prefix n parameters), (caller_saved @ [RA;V0;A0;A1;A2]
(*  TODO : laisser RA et V0 ? *)
  | Esyscall _ -> [V0; A0], [V0]
  | Ealloc_frame _ -> [], []
  | Edelete_frame _ -> [], []
  | Eget_stack_param(r,_,_) -> [], [r] 
  | Eset_stack_param(r,_,_) -> [r], [] 
  | Einit_addr(r,_,_) -> [], [r]
  | Emove(r1,r2,_) -> [r1], [r2]
  | ELi(r,_,_) -> [], [r]
  | ELa(r,a,_) -> (list_of_address a), [r]
  | ELw(r,a,_) -> (list_of_address a), [r]
  | ESw(r,a,_) -> ([r] @ (list_of_address a)), []
  | ELb(r,a,_) -> (list_of_address a), [r]
  | ESb(r,a,_) -> ([r] @ (list_of_address a)), []
  | EAddress(r1,r2,_) -> [], [r1]
  | EArith(_,r1,r2,Rtl.Oimm(_),_) -> [r2], [r1]
  | ESet(_,r1,r2,Rtl.Oimm(_),_) -> [r2], [r1]
  | EArith(_,r1,r2,Rtl.Oreg(r3),_) -> [r2; r3], [r1]
  | ESet(_,r1,r2,Rtl.Oreg(r3),_) -> [r2; r3], [r1]
  | ENeg(r1,r2,_) -> [r2], [r1]
  | Egoto (_) -> [], []
  | EBeq(r1,r2,_,_) -> [r1;r2], []
  | EBeqz (r,_,_) -> [r], []
  | EBnez (r,_,_) -> [r], []
  | EJr(r) -> [r], []
  | ELoop_begin _ -> [], []
  | ELoop_end _ -> [], []
  | EReturn -> (result::ra::callee_saved), []

let rec from_list = function
  | [] -> Rset.empty
  | h::t -> Rset.add h (from_list t)

let use_or_def instr =
  let (use,def) = use_def instr in
  from_list (use @ def)

module Lmap = Map.Make(struct type t=Rtl.label
  let compare = compare end)

module Lset = Set.Make(struct type t=Rtl.label
  let compare = compare end)

module Rmap = Map.Make(struct type t=Register.register
  let compare = compare end)

let uses = ref Lmap.empty
let predecesseurs = ref Lmap.empty
let voisins_succ = ref Lmap.empty
let is_in_loop = ref Lset.empty
let uses_statistics = ref Rmap.empty

type statistics = (int * int) Rmap.t

let incr_count reg loop =
  let (curr_loop,curr_not_loop) =
    try
      Rmap.find reg !uses_statistics
    with Not_found -> (0,0)
  in
  let nv =
    if loop then
      (curr_loop+1,curr_not_loop)
    else (curr_loop, curr_not_loop+1) in
  uses_statistics := Rmap.add reg nv !uses_statistics

let add_pred map new_pred lbl =
  let current_set =
    try
      Lmap.find lbl !map
    with Not_found -> Lset.empty
  in
  map := Lmap.add lbl (Lset.add new_pred current_set) !map

let print_lset f s =
  Lset.iter (Print_rtl.p_label f) s

let print_lset_lmap f m =
  Lmap.iter (fun lbl s -> Format.fprintf f
             "%a : %a\n" Print_rtl.p_label lbl print_lset s) m

let find_or_empty key map =
  try
    Lmap.find key map
  with Not_found -> Lset.empty

let calcul_pred_succ g =
  let voisins_pred = ref Lmap.empty in
  voisins_succ := Lmap.empty;
  predecesseurs := Lmap.empty;
  Ertl.M.iter (fun lbl instr ->
    let lst = Ertl.successeurs instr in
    List.iter (add_pred voisins_pred lbl) lst;
    List.iter (fun x -> add_pred voisins_succ x lbl) lst) g;
  let in_loop lbl =
    match M.find lbl g with
    | ELoop_begin _ -> true
    | ELoop_end _ -> false
    | _ -> Lset.mem lbl !is_in_loop
  in
  let set_in_loop lbl =
    match M.find lbl g with
    | ELoop_begin _ -> ()
    | _ -> is_in_loop := Lset.add lbl !is_in_loop
  in
  let rec dfs dejavu res voisins start =
    try
      Lmap.find start !res
    with Not_found ->
      (if dejavu.(start) then Lset.empty
      else
        begin
          let preds = find_or_empty start voisins in
          let accu = ref preds in
          dejavu.(start) <- true;
          Lset.iter
            (fun x ->
              if in_loop x then
                set_in_loop start;
              accu := Lset.union !accu (dfs dejavu res voisins x))
            preds;
          res := Lmap.add start !accu !res;
          !accu
      end) 
  in
  (* Pour chaque instruction *)
  Ertl.M.iter (fun lbl instr ->
    (* On calcule les prédécesseurs de lbl *)
    let _ = dfs (Array.make (Rtl.max_label ()) false) predecesseurs
      !voisins_pred lbl in
    (* On met à jour le nombre d'occurences des variables en jeu *)
    Rset.iter (fun reg -> incr_count reg (Lset.mem lbl !is_in_loop))
      (use_or_def instr)
  ) g

let get_in_out cur_uses lbl =
  try
    Lmap.find lbl cur_uses
  with Not_found -> (Rset.empty,Rset.empty)

let get_in cur_uses lbl = fst (get_in_out cur_uses lbl)
let get_out cur_uses lbl = snd (get_in_out cur_uses lbl)

let p_rset f s =
  Print_rtl.p_list "," Print_rtl.p_pseudoreg f (Register.Rset.elements s)

let kildall g =
  predecesseurs := Lmap.empty;
  voisins_succ := Lmap.empty;
  uses := Lmap.empty;
  uses_statistics := Rmap.empty;
  calcul_pred_succ g;
  let working_list = ref Lset.empty in

  (* On ajoute tous les labels dans la working_list *)
  Ertl.M.iter 
  (fun lbl instr -> working_list := Lset.add lbl !working_list) g; 

  (* Tant que la working_list n'est pas vide *)
  while not (Lset.is_empty !working_list) do
    (* On en prend un élément *)
    let lbl = Lset.choose !working_list in
    working_list := Lset.remove lbl !working_list;
    (* On récupère la valeur précédente de in(lbl) *)
    let old_in = get_in !uses lbl in
    (* On calcule le nouveau out(lbl) *)
    let new_out = Lset.fold
      (fun succ accu -> Rset.union accu (get_in !uses succ)) 
      (find_or_empty lbl !voisins_succ) Rset.empty in
    (* On calcule le nouveau in(lbl) *)
    let (use,def) = use_def (Ertl.M.find lbl g) in
    let new_in = Rset.union (from_list use)
      (Rset.diff new_out (from_list def)) in
    (* Si in(lbl) != old_in *)
    if not (Rset.equal new_in old_in) then
      begin
        uses := Lmap.add lbl (new_in,new_out) !uses;
        (* On ajoute à la working_list tous les prédécesseurs de lbl *)
        working_list := Lset.union !working_list
          (Lmap.find lbl !predecesseurs)
    end
  done

type liveness = (Rset.t * Rset.t) Lmap.t

type decl =
  { name : string; nb_args : int; g : Ertl.graph; entry : Rtl.label;
    su_size : int;
    uses : liveness;
    statistics : (int*int) Rmap.t }

let rec compute_uses = function
  | [] -> []
  | d::t ->
    kildall d.Ertl.g; 
    let uses_copy = !uses in
    let statistics_copy = !uses_statistics in
    {name = d.Ertl.name;
      nb_args = d.Ertl.nb_args;
      g = d.Ertl.g;
      entry = d.Ertl.entry;
      su_size = d.Ertl.su_size;
      uses = uses_copy;
      statistics = statistics_copy }::(compute_uses t)

