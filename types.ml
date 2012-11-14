
(* Définition des arbres de types *)

type expr_type =
    | ET_void
    | ET_int
    | ET_char
    | ET_struct of string
    | ET_union of string
    | ET_star of expr_type
    | ET_null


