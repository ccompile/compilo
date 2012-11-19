
open Ast

type expr_type =
    | ET_void
    | ET_int
    | ET_char
    | ET_struct of string
    | ET_union of string
    | ET_star of expr_type
    | ET_null


module EnvType = Map.Make(struct type t=string
let compare = compare end)

type env_expr = (expr_type) EnvType.t

let num_type = function
    | ET_int | ET_char | ET_null -> true
    | _ -> false

let pointer_type = function
    | ET_null | ET_star _ -> true
    | _ -> false


type tident = expr_type * aident 

type wexpr =
    | TE_int of int
    | TE_str of string
    | TE_ident of aident
    | TE_star of texpr
    | TE_brackets of texpr * texpr
    | TE_dot of texpr * aident
    | TE_arrow of texpr * aident
    | TE_gets of texpr * texpr
    | TE_call of aident * (texpr list)
    | TE_incr of aincr * texpr
    | TE_unop of aunop * texpr
    | TE_binop of abinop * texpr * texpr
    | TE_sizeof of int 
and texpr = expr_type * wexpr


