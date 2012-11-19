
open Ast
open Types
open Errors

let rec is_lvalue = function
    | AE_ident _ -> true
    | AE_star _ -> true
    | AE_dot ((_,e),_) -> is_lvalue e
    | _ -> true

    (* Met à jour l'environnement et renvoie le type *)
let rec type_expr env (lbl,expr) = match expr with
    (* TODO *)
    | AE_int x -> (ET_int, TE_int x)
    | AE_str x -> (ET_star (ET_char), TE_str x)
    | AE_ident s ->
            let typ =
            try
                EnvType.find s env
            with Not_found ->
                raise (Typing_error
                (lbl,Printf.sprintf "Undefined identifier %s"s))
            in
            (typ, TE_ident s)
    | AE_star x ->
            let (et,te) = type_expr env x in
            (ET_star et, TE_star (et,te))           
    | _ -> (ET_void, TE_int 0) 



    (* Met à jour l'environnement et renvoie le type *)
let type_instr env = function
    (* TODO *)
    | AI_none -> ET_void
    | _ -> ET_void

    (* Met à jour l'environnement sans renvoyer de type *)
let type_decl env (lbl,decl) = match decl with
    (* TODO *)
    | Adecl_vars _ -> ()
    | _ -> ()

let type_ast ast =
    let env = [] in
    List.iter (type_decl env) ast

