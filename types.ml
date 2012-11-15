
type expr_type =
    | ET_void
    | ET_int
    | ET_char
    | ET_struct of string
    | ET_union of string
    | ET_star of expr_type
    | ET_null


let num_type = function
    | ET_int | ET_char | ET_null -> true
    | _ -> false

let pointer_type = function
    | ET_null | ET_star _ -> true
    | _ -> false




