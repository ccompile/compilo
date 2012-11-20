
open Ast
open Types
open Errors

(** FONCTIONS GÉNÉRIQUES **)

    (* Vérifie le type d'une liste d'éléments avec le typeur fourni *)
let rec type_list typer env = function
    | [] -> ()
    | h::t -> typer env h; type_list typer env t

    (* Renvoie l'environnement mis à jour après lecture de la liste fournie *)
let rec update_env typer env = function
    | [] -> env
    | h::t -> update_env typer (typer env h) t

(** PROPRIÉTÉS DES TYPES **)
    
    (* Cette expression est-elle une valeur gauche ? *)
let rec is_lvalue = function
    | AE_ident _ -> true
    | AE_star _ -> true
    | AE_dot ((_,e),_) -> is_lvalue e
    | _ -> true

    (* Ces deux types sont-ils compatibles ? *)
let compatible a b =
    (a = b)
    || ((a = ET_int || a = ET_char || a = ET_null) &&
        (b = ET_int || b = ET_char || b = ET_null))
    || (match a,b with
          | ET_null,ET_star _
          | ET_star _, ET_null
          | ET_star _, ET_star ET_void  
          | ET_star ET_void, ET_star _ -> true
          | _ -> false)

    (* Ce type représente-t-il un nombre ? *)
let is_num t =
    (compatible t ET_int) || (compatible t (ET_star ET_void))

(** TYPAGE DES EXPRESSIONS **)

    (* Renvoie l'arbre étiqueté par ses types *)
let rec type_expr env (lbl,expr) = match expr with
    | AE_int 0 -> (ET_null, TE_int 0)
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
    | AE_gets (lhs,rhs) -> 
            let (etl,tel) = type_expr env lhs in
            if not (is_lvalue (snd lhs)) then
                raise (Typing_error
                (lbl,"lvalue required as left operand of assignment"));
            let (etr,ter) = type_expr env rhs in
            if not (compatible etl etr) then
                raise (Typing_error
                (lbl,Printf.sprintf
                "incompatible types when assigning to type `%s' from type `%s'"
                (string_of_type etl) (string_of_type etr)));
            (etl, TE_gets ((etl,tel), (etr,ter)))
    (* TODO *)
    | _ -> (ET_void, TE_int 0) 


    (* Renvoie le type représenté par l'expression de type correspondante *)
let rec type_type env x = match x with
    | A_void -> ET_void
    | A_int -> ET_int
    | A_char -> ET_char
    (* TODO : check that s is a valid identifier *)
    | A_struct s -> ET_struct (snd s)
    | A_union s -> ET_union (snd s)

(** AJOUT D'IDENTIFIEURS À L'ENVIRONNEMENT *)

    (* Renvoie le type et l'identifiant de la variable déclarée *)
let rec type_and_id_of_avar env basetype = function
    | AV_ident (_,s) -> (type_type env basetype, s)
    | AV_star x -> let (t,s) = (type_and_id_of_avar env basetype x) in
                    (ET_star t, s)

let add_avar_to basetype env v =
    let (value,key) = type_and_id_of_avar env basetype v in
    EnvType.add key value env

    (* Met à jour l'environnement après déclaration de variables *)
                    (* TODO : remove ldecl_vars *)
let type_declvar env (lb,((lbl,basetype),lst)) =
    List.fold_left (add_avar_to basetype) env lst

let type_arguments env (lb,((lbl,basetype),var)) =
    add_avar_to basetype env var

(** VÉRIFICATION DES INSTRUCTIONS, DÉCLARATIONS **)

    (* Vérifie le type des composants de l'instruction. *)
let type_instr env (lbl,instr) = match instr with
    | AI_none -> ()
    | AI_inst x -> let _ = type_expr env (lbl,x) in ()
    | AI_return(Some x) ->
            let rettype = (type_expr env x) in
            (* TODO : check that the rettype is correct *)
            ()
    (* TODO *)
    | _ -> ()

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
    (* TODO *)
    | _ -> ()

let type_ast ast =
    let env = EnvType.empty in
    List.iter (type_decl env) ast

