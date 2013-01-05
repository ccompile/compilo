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
    with Not_found -> { prefs = Rset.empty; intfs = Rset.empty }

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


let remove_from_graph reg =
    graphe := M.remove reg !graphe;
    M.iter (fun r arc ->
           graphe := M.add r {prefs = Rset.remove reg arc.prefs;
           intfs = Rset.remove reg arc.intfs} !graphe) !graphe

(* This is a workaround to handle the problem of reference evaluation *)
exception Simplification_found
(* When we simplify one vertex, then we must update the graph, but
 * the graph we're M.itering on doesn't change, so we must stop the computation
 * by raising an exception and start again until no simplification is found *)
(* Problem : time complexity *)

let rec simplify () =
    let simplify_one () =
        M.iter
        (fun reg arc ->
            if Rset.cardinal arc.intfs < max_deg &&
               Rset.is_empty arc.prefs then
               begin
                   simplified_stack := (reg,arc)::!simplified_stack;
                   remove_from_graph reg;
                   raise Simplification_found
               end) !graphe
     in
     try simplify_one ()
     with Simplification_found -> simplify_one ()

let replace_in set old_v new_v =
    if Rset.mem old_v set then
        Rset.add new_v (Rset.remove old_v set)
    else set

let rec coalesce () =
    let merge_vertices v1 v2 =
        let arc1 = find_or_empty v1 !graphe in
        let arc2 = find_or_empty v2 !graphe in
        let new_graph = M.remove v2 !graphe in
        M.iter
        (fun reg arc ->
            graphe := M.add reg {prefs = replace_in arc.prefs v2 v1;
                                 intfs = replace_in arc.intfs v2 v1} !graphe)
        new_graph;
        graphe := M.add v1
         {
        

    let coalesce_one () =
        M.iter
        (fun reg arc ->
            if not (Rset.is_empty arc.prefs) then
              Rset.iter
              (fun reg2 ->
                  let arc2 = find_or_empty reg2 !graphe in
                  if Rset.cardinal arc2.intfs + (Rset.cardinal arc.intfs) <
                  max_deg && (not (Rset.mem reg2 arc.intfs)) then
                   begin
                       merge_vertices reg reg2;
                       raise Simplification_found
                   end) !graphe)
        !graphe
    in
    try coalesce_one ()
    with Simplification_found -> coalesce_one ()



