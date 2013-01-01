
(* Calcule la taille et la position de chaque
 * champ dans la structure déclarée, associée à son nom *)
val declare_type : string -> Types.type_signature -> unit

(* Accède au décalage (en octets) du champ
 * dans le type donné *)
val get_offset : Types.expr_type -> string -> int

(* Accède à la taille (en octets) du type donné.
 * Lève Not_found si le type n'a pas été correctement défini *)
val get_sizeof : Types.expr_type -> int

