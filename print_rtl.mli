
val p_decl_list : Format.formatter -> Rtl.decl list -> unit

val print_rtl : Format.formatter -> Rtl.graph -> unit

val p_pseudoreg : Format.formatter -> Register.register -> unit

val p_address : Format.formatter -> Rtl.address -> unit

val p_operand : Format.formatter -> Rtl.operand -> unit

val p_label : Format.formatter -> Rtl.label -> unit

val p_list : string -> (Format.formatter -> 'a -> unit) -> Format.formatter
 -> 'a list -> unit

