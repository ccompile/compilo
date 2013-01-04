open Ertl
open Register
open Rtl
open Kildall

module M = Map.Make(struct type t=register let compare= compare end)

type arcs = { prefs : Register.set; intfs : Register.set }
type graph = arcs M.t

let make f =
  let Fct(name,arg,graph,lab,set,entreesortie) = f in
  (* TODO : Parcourir le graphe ERTL slide 11 dernier cours
  ISSUE: Comment éviter les boucles, faire une table dejavu?*)  
