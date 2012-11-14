
(* Déclaration des types de l'arbre de sytaxe abstraite *)

type label = { file: string; line : int; cbegin : int; cend : int }

type aident = string
type lident = label*aident

type atype = A_void | A_int | A_char | A_struct of lident | A_union of lident 
type ltype = label*atype

type avar = AV_ident of lident | AV_star of avar

type adecl_vars = ltype * (avar list)
and ldecl_vars = label * adecl_vars

type aincr = 
    | IncrRet (* ++a *)
    | DecrRet (* --a *)
    | RetIncr (* a++ *)
    | RetDecr (* a-- *)
and lincr = label * aincr

type aunop =
    | AU_addr (* & *)
    | AU_not  (* ! *)
    | AU_minus(* - *)
    | AU_plus (* + *)
and lunop = label * aunop

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
and lbinop = label * abinop

type aexpr =
    | AE_int of int
    | AE_str of string
    | AE_ident of aident
    | AE_star of lexpr
    | AE_brackets of lexpr * lexpr
    | AE_dot of lexpr * lident
    | AE_arrow of lexpr * lident
    | AE_gets of lexpr * lexpr
    | AE_call of lident * (lexpr list)
    | AE_incr of aincr * lexpr
    | AE_unop of aunop * lexpr
    | AE_binop of abinop * lexpr * lexpr
    | AE_sizeof of ltype * int (* nombre d'étoiles *)
and lexpr = label * aexpr

type ainstr =
    | AI_none               (* ; *)
    | AI_inst of aexpr       (* <expr> ;  (instantiation) *)
    | AI_if of lexpr * linstr
    | AI_if_else of lexpr * linstr * linstr
    | AI_while of lexpr * linstr 
    | AI_for of (lexpr list) * lexpr option * (lexpr list) * linstr
    | AI_bloc of abloc
    | AI_return of lexpr option
and abloc = (ldecl_vars list) * (linstr list)
and linstr = label * ainstr
and lbloc = label * abloc

type aargument = ltype * avar
and largument = label * aargument

type adecl =
 | Adecl_vars of adecl_vars
 | Adecl_typ of (bool (* is_union *) * lident * (ldecl_vars list))
 | Adecl_fct of (ltype * int (* nombre d'étoiles *)
                * lident * (largument list) * lbloc)
and ldecl = label * adecl

type afichier = ldecl list


