open Ertl
open Register
open Rtl
open Kildall

module M = Map.Make(struct type t=register let compare= compare end)

type arcs = { prefs : Register.set; intfs : Register.set }
type graph = arcs M.t
let graphe = ref M.empty
let simplified_stack = ref []
let max_deg = 42 (* TODO *)

let find_or_empty key map =
    try
        M.find key map
    with Not_found -> { prefs = Rset.empty; intfs = Rset.empty }

let make f =
  let Fct(name,arg,graph,lab,set,entreesortie) = f in
  let todo instr label = match instr with
    |instr when (snd (use_def instr) != []) -> 
    let definis = snd(use_def instr) in
    let (ensentr,enssort) = Kildall.Lmap.find label entreesortie in
  (*find doesn't raise Not_found, by construction in kildall*) 
      let todo_1_reg_cree reg=
        match instr with
            |Ertl.Emove(w,reg,_)->
                let intf = Register.Rset.remove reg enssort in
                let intf = Register.Rset.remove w intf  in
                let ancienint= (find_or_empty reg !graphe).intfs in
                let ancienpref= (find_or_empty reg !graphe).prefs in
                let ancienpref= Register.Rset.union ancienpref
                                        (Register.Rset.singleton w) in
                let intf = Register.Rset.union intf ancienint in
                graphe:= M.remove reg (!graphe);
                graphe:= M.add reg {prefs=ancienpref;intfs=intf} (!graphe);  

            |_-> let intf=Register.Rset.remove reg enssort  in
                 let ancienint= (find_or_empty reg !graphe).intfs in
                 let ancienpref= (find_or_empty reg !graphe).prefs in
                 let intf = Register.Rset.union intf ancienint in
                 graphe:= M.remove reg (!graphe);
                 graphe:= M.add reg {prefs=ancienpref;intfs=intf} (!graphe);  
      in
      List.iter todo_1_reg_cree definis ;
    |_-> ()
  in
  Ertl.M.iter (fun x y->todo y x) graph


let precolored = ref Rset.empty
let initial = ref Rset.empty
let simplify_worklist = ref Rset.empty
let freeze_worklist = ref Rset.empty
let spill_worklist = ref Rset.empty
let spilled_nodes = ref Rset.empty
let coalesced_nodes = ref Rset.empty
let colored_nodes = ref Rset.empty
let select_stack = ref []

module Mset = Set.Make(struct type t = (Register.register*Register.register)
 let compare = compare end)

let coalesced_moves = ref Mset.empty
let constrained_moves = ref Mset.empty
let frozen_moves = ref Mset.empty
let worklist_moves = ref Mset.empty
let active_moves = ref Mset.empty

let adj_set = Hashtbl.create 17
let adj_list = ref M.empty
let degree = ref M.empty
let infty_deg = ref 0 (* TODO : initialize infty_deg and make accessors for
'degree' *)

let move_list = ref M.empty
let alias = ref M.empty
let color = ref M.empty

let add_edge u v =
    if (not (Hashtbl.

