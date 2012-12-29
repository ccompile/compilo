(*Register*)
module S : Set.S with type elt = t
type t

val fresh : unit -> t

(*Label*)
module M : Map.S with type key = t
val fresh : unit -> t
type t


let glob_reg = ref 0 
let fresh () = incr glob_reg; (!glob_reg -1)



type graph = instr Label.M.t

let graph = ref Label.M.empty

let generate i =
  let l = Label.fresh () in
  graph := Label.M.add l i !graph;
  l

