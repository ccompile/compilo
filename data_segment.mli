
val declare_global : string (* name *) -> Types.expr_type -> unit (* label *)

val declare_string : string (* value *) -> string (* label*)

val get_data : unit -> Mips.data list

val get_global_label : string (* name *) -> string (* label *)

(* TODO : check that this label has been allocated *)

