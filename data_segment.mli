
val declare_global : string (* name *) -> Types.expr_type -> string (* label *)

val declare_string : string (* name *) -> string (* value *) -> string (* label*)

val get_data : unit -> Mips.data list


