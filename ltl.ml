open Ertl
open Register
open Rtl
open Kildall

module M = Map.Make(struct type t=register let compare= compare end)

type arcs = { prefs : Register.set; intfs : Register.set }
type graph = arcs M.t
let graphe = ref M.empty


let make f =
  let Fct(name,arg,graph,lab,set,entreesortie) = f in
  (* TODO : Parcourir le graphe ERTL slide 11 dernier cours
  ISSUE: Comment éviter les boucles, faire une table dejavu?*)
  let todo instr label = match instr with
    |instr when (snd (use_def instr) != []) -> 
    let definis = snd(use_def instr) in
    let (ensentr,enssort) = Kildall.Lmap.find label entreesortie in
  (*find don't raise Not_found, by construction in kildall*) 
      let todo_1_reg_cree reg=
        match instr with
            |Ertl.Emove(w,reg,_)->
                let intf = Register.Rset.remove reg enssort in
                let intf = Register.Rset.remove w intf  in
                let ancienint= (M.find reg (!graphe) ).intfs in
                let ancienpref= (M.find reg (!graphe)).prefs in
                let ancienpref= Register.Rset.union ancienpref
                                        (Register.Rset.singleton w) in
                let intf = Register.Rset.union intf ancienint in
                graphe:= M.remove reg (!graphe);
                graphe:= M.add reg {prefs=ancienpref;intfs=intf} (!graphe);  

            |_-> let intf=Register.Rset.remove reg enssort  in
                 let ancienint= (M.find reg (!graphe) ).intfs in
                 let ancienpref= (M.find reg (!graphe)).prefs in
                 let intf = Register.Rset.union intf ancienint in
                 graphe:= M.remove reg (!graphe);
                 graphe:= M.add reg {prefs=ancienpref;intfs=intf} (!graphe);  
      in
      List.iter todo_1_reg_cree definis ;
    |_-> ()
  in
  Ertl.M.iter (fun x y->todo y x;) graph

