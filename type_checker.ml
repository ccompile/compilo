
open Ast
open Types
open Errors

(* Cette expression est-elle une valeur gauche ? *)
let rec is_lvalue = function
    | AE_ident _ -> true
    | AE_star _ -> true
    | AE_dot ((_,e),_) -> is_lvalue e
    | _ -> true

    (* Vérifie le type d'une liste d'éléments avec le typeur fourni *)
let rec type_list typer env = function
    | [] -> ()
    | h::t -> typer env h; type_list typer env t

    (* Renvoie l'environnement mis à jour après lecture de la liste fournie *)
let rec update_env typer env = function
    | [] -> env
    | h::t -> update_env typer (typer env h) t

    (* Renvoie l'arbre étiqueté par ses types *)
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

    (* Renvoie l'arbre étiqueté par ses types *)
let type_instr env (lbl,instr) = match instr with
    (* TODO *)
    | AI_none -> ET_void
    | AI_return(Some x) -> fst (type_expr env x)
    | _ -> ET_void

    (* Renvoie le type représenté par l'expression de type correspondante *)
let rec type_type env x = match x with
    | A_void -> ET_void
    | A_int -> ET_int
    | A_char -> ET_char
    (* TODO : struct & union *)
    | _ -> ET_void

    (* Renvoie le type et l'identifiant de la variable déclarée *)
let rec type_and_id_of_avar env basetype = function
    | AV_ident (_,s) -> (type_type env basetype, s)
    | AV_star x -> let (t,s) = (type_and_id_of_avar env basetype x) in
                    (ET_star t, s)


    (* Met à jour l'environnement après déclaration de variables *)
                    (* TODO : remove ldecl_vars *)
let type_declvar env (lb,((lbl,basetype),lst)) =
    let add_to env v =
        let (value,key) = type_and_id_of_avar env basetype v in
        EnvType.add key value env
    in
    List.fold_left add_to env lst

let type_bloc env (lbl,(dvars,dinstr)) =
        let nenv = update_env type_declvar env dvars in
        let _ = type_list type_instr nenv dinstr in ()

    (* Met à jour l'environnement sans renvoyer de type,
     * mais en vérifiant que tout est bien typé *)
let type_decl env (lbl,decl) = match decl with
    (* TODO *)
    | Adecl_vars _ -> ()
    | Adecl_fct (ret, nbs, name, args, body) ->
            type_bloc env body 
    | _ -> ()

let type_ast ast =
    let env = EnvType.empty in
    List.iter (type_decl env) ast

