
type color =
  | Reg of Register.register
  | Stack of int

val allocate_registers : Ertl.graph -> Kildall.liveness -> unit

val get_color : Register.register -> color

