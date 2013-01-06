
type color =
  | Reg of Register.register
  | Stack of int

type coloring

val allocate_registers : Ertl.graph -> Kildall.liveness -> coloring

val get_color : coloring -> Register.register -> color

val print_color : Format.formatter -> color -> unit

val print_coloring : Format.formatter -> coloring -> unit

