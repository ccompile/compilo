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





type instr=
  | Lcall of string*int*label
  | Lsyscall of label
  | Lalloc_frame of label
  | Ldelete_frame of label
  | Lget_stack of register*int*label
  | Lset_stack of register*int*label
(*Suite ne change pas de précedemment*)
  | Lmove of register*register*label
  | LLi   of register * int32 * label
  | LStr   of register * string * label
  | LLw   of register * address * label
  | LSw   of register * address * label
  | LLb   of register * address * label
  | LSb   of register * address * label
  | LAddress of register * int * register * label
  | LArith of Mips.arith * register * register * operand * label
  | LSet of Mips.condition * register* register* operand* label
  | LNeg of register * register* label
  | Lgoto   of label
  | LBeq  of register * register * label * label
  | LBeqz of register * label * label
  | LBnez of register * label * label
  | LJr   of register
  | LReturn  

let tmp1, tmp2 = "$v1", "$fp"

let write c r l = match lookup c r with
  | Reg hr -> hr, l
  | Spilled n -> tmp1, generate (Eset_stack (tmp1, n, l))

let read1 c r f = match lookup c r with
  | Reg hr -> f hr
  | Spilled n -> Eget_stack (tmp1, n, generate (f tmp1))

let instr c frame size = function
  | Ertl.Econst (r, n, l) ->
      let hwr, l = write c r l in
      Econst (hwr, n, l)
  | Ertl.Eaccess_global (r, x, l) ->
      let hwr, l = write c r l in
      Eaccess_global (hwr, x, l)
  | Ertl.Embinop (r1, op, r2, r3, l) ->
    read2 c r2 r3 (fun hw2 hw3 ->
    let hw1, l = write c r1 l in
    Embinop (hw1, op, hw2, hw3, l))
  | Ertl.Eget_stack_param (r, n, l) ->
    let hwr, l = write c r l in
    Eget_stack (hwr, frame_size + n, l)
  | Ertl.Eset stack_param (r, n, l) ->
    read1 c r (fun hwr -> Eset stack (hwr, n, l))
  | Ertl.Ealloc_frame l
  | Ertl.Edelete_frame l when frame_size = 0 ->
    Egoto l 
  | Ertl.Ealloc_frame l ->
    Emunop (Register.sp, Maddi (-frame_size), Register.sp, l)
  | Ertl.Edelete_frame l ->
    Emunop (Register.sp, Maddi frame_size, Register.sp, l)



let deffun t =
  (*let ln = Liveness.analyze f.Ertl.fun_body in
  let ig = Interference.make ln in
  let c, nlocals = Coloring.find ig in
   CECI PERMET DE GENERER TOUT JUSQU'AU COLORIAGE*)
  let n stack params =
    max 0 (f.Ertl.fun_formals-List.length Register.parameters)
  in
  let frame size = word size * (nlocals + n stack params) in
  graph := Label.M.empty;
  Label.M.iter (fun l i ->
    let i = instr c frame size i in
    graph := Label.M.add l i !graph)
    f.Ertl.fun_body;
  { fun name = f.Ertl.fun_name;
    fun entry = f.Ertl.fun_entry;
    fun body = !graph; }


