
val print_graph_dot : bool ref

type color =
  | Reg of Register.register
  | Stack of int

type coloring

val allocate_registers : Ertl.graph -> Kildall.liveness -> Kildall.statistics -> coloring

val get_color : coloring -> Register.register -> color

val spilled_count : coloring -> int

val print_color : Format.formatter -> color -> unit

val print_coloring : Format.formatter -> coloring -> unit

