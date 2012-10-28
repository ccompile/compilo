
(* Déclaration des types de l'arbre de sytaxe abstraite *)

type aident = string

type atype = A_void | A_int | A_char | A_struct of aident | A_union of aident 

type avar = AV_ident of aident | AV_star of avar

type adecl_vars = atype * (avar list)

type aincr = 
    | IncrRet (* ++a *)
    | DecrRet (* --a *)
    | RetIncr (* a++ *)
    | RetDecr (* a-- *)

type aunop =
    | AU_addr (* & *)
    | AU_not  (* ! *)
    | AU_minus(* - *)
    | AU_plus (* + *)

type abinop =
    | AB_equal(* == *)
    | AB_diff (* != *)
    | AB_lt   (* <  *)
    | AB_leq  (* <= *)
    | AB_gt   (* >  *)
    | AB_geq  (* >  *)
    | AB_plus (* +  *)
    | AB_minus(* -  *)
    | AB_times(* *  *)
    | AB_div  (* /  *)
    | AB_mod  (* %  *)
    | AB_and  (* && *)
    | AB_or   (* || *)

type aexpr =
    | AE_int of int
    | AE_str of string
    | AE_ident of aident
    | AE_star of aexpr
    | AE_brackets of aexpr * aexpr
    | AE_dot of aexpr * aexpr
    | AE_arrow of aexpr * aident
    | AE_gets of aexpr * aexpr
    | AE_call of aident * (aexpr list)
    | AE_incr of aincr * aexpr
    | AE_unop of aunop * aexpr
    | AE_binop of abinop * aexpr * aexpr
    | AE_sizeof of atype * int (* nombre d'étoiles *)

type ainstr =
    | AI_none               (* ; *)
    | AI_inst of aexpr       (* <expr> ;  (instantiation) *)
    | AI_if of aexpr * ainstr
    | AI_if_else of aexpr * ainstr * ainstr
    | AI_while of aexpr * ainstr 
    | AI_for of (aexpr list) * aexpr option * (aexpr list) * ainstr
        (* TODO : ligne précédente à corriger ? *)
    | AI_bloc of abloc
    | AI_return of aexpr option
and abloc = (adecl_vars list) * (ainstr list)

type aargument = atype * avar

type adecl =
 | Adecl_vars of adecl_vars
 | Adecl_typ of (bool (* is_union *) * aident * (adecl_vars list))
 | Adecl_fct of (atype * int (* nombre d'étoiles *)
                * aident * (aargument list) * abloc)

type afichier = adecl list


