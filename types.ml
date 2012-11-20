
open Ast

type expr_type =
    | ET_void
    | ET_int
    | ET_char
    | ET_struct of string
    | ET_union of string
    | ET_star of expr_type
    | ET_null


module Env = Map.Make(struct type t=string
let compare = compare end)

type field = expr_type * string

type type_signature =
    | UnionSig of field list
    | StructSig of field list

type env_expr = (expr_type) Env.t
type env_sig = (type_signature) Env.t

let num_type = function
    | ET_int | ET_char | ET_null -> true
    | _ -> false

let pointer_type = function
    | ET_null | ET_star _ -> true
    | _ -> false

let rec string_of_type = function
    | ET_void -> "void"
    | ET_int -> "int"
    | ET_char -> "char"
    | ET_struct s -> Printf.sprintf "struct %s" s
    | ET_union s -> Printf.sprintf "union %s" s
    | ET_star e -> Printf.sprintf "%s*" (string_of_type e)
    | ET_null -> "nulltype"

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


