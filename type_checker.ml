open Ast
open Types
open Errors

(** FONCTIONS GÉNÉRIQUES **)

    (* Environnement global stockant les déclarations de type *)
let sig_env = ref Env.empty

    (* Environnement global stockant les déclarations de fonctions *)
let proto_env = ref Env.empty

    (* Environnement global stockant les déclarations de… globales ! *)
let globals_env = ref Env.empty

    (* Fonctions prédéfinies ("builtins") *)
let add_builtins () =
    let sbrk =    { return = ET_star ET_void ;
                    name = "sbrk";
                    args = [ET_int] } in
    let putchar = { return = ET_int ;
                    name = "putchar";
                    args = [ET_int] } in 
    proto_env := Env.add "sbrk" sbrk !proto_env;
    proto_env := Env.add "putchar" putchar !proto_env


let () = add_builtins ()

    (* Vérifie le type d'une liste d'éléments avec le typeur fourni *)
let rec type_list typer env =
    List.map (typer env)

    (* Renvoie l'environnement mis à jour après lecture de la liste
fournie *)
let rec update_env typer env = function
    | [] -> (env,[])
    | h::t -> let (nenv1,typer_result) = (typer env h) in
              let (nenv2,nt) = update_env typer nenv1 t in
              (nenv2,typer_result::nt)              

(** PROPRIÉTÉS DES TYPES **)
    
    (* Cette expression est-elle une valeur gauche ? *)
let rec is_lvalue = function
    | AE_ident _ -> true
    | AE_star _ -> true
    | AE_dot ((_,e),_) -> is_lvalue e
    | AE_arrow((_,e),_) -> true
    | AE_brackets((_,e),_) -> true
    | _ -> false

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

    (* Renvoie le type du champ id dans une liste de champs *)
let rec get_field_type id = function
    | [] -> raise Not_found
    | (typ,field)::t when field = id -> typ
    | h::t -> get_field_type id t

(** TYPAGE DES EXPRESSIONS **)

    (* Renvoie l'arbre étiqueté par ses types *)
let rec type_expr env (lbl,expr) = match expr with
    | AE_int 0 -> (ET_null, TE_int 0)
    | AE_int x -> (ET_int, TE_int x)
    | AE_char c -> (ET_char, TE_char c)
    | AE_str x -> (ET_star (ET_char), TE_str x)
    | AE_ident s ->
            let typ =
            try
                Env.find s env
            with Not_found ->
                raise (Typing_error
                (lbl,Printf.sprintf "Undefined identifier %s"s))
            in
            (typ, TE_ident s)
    | AE_star x ->
            let (et,te) = type_expr env x in
            (match et with
             | ET_star net -> (net, TE_star (et,te))
             | _ -> raise (Typing_error (lbl,"Only pointers can be dereferenced"
             ^", and this value has type `"^(string_of_type et)^"'")))         
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
(* La syntaxe a[b] est équivalente à *(a+b) mais traiter ce cas
* séparément permet de renvoyer des messages d'erreur plus
* explicites *)
    | AE_brackets (lhs, rhs) ->
            let (etl,tel) = type_expr env lhs in
            (match etl with
             | ET_star t ->
                 let (etr,ter) = type_expr env rhs in
                 if not (is_num etr) then
                     raise (Typing_error
                     (lbl,"array subscript is not an integer"));
                 (t, TE_brackets ((etl,tel),(etr,ter)))
             | _ -> raise (Typing_error
                    (lbl,"subscripted value is neither array nor "^
                    "pointer")))
    | AE_dot (lhs,fld) ->
            let (etl,tel) = type_expr env lhs in
            (try
                (match etl with
                  | ET_union id
                  | ET_struct id ->
                          let typesig =
                              try
                                  Env.find id !sig_env
                              with Not_found -> assert false in
                          (match typesig with
                            | UnionSig lst
                            | StructSig lst ->
                                    let ft = get_field_type (snd fld) lst
                                    in
                                    (ft, TE_dot ((etl,tel),snd fld)))
                  | _ -> raise Not_found)
            with Not_found ->
                raise (Typing_error (lbl,
                Printf.sprintf "type `%s' has no field named `%s'"
                (string_of_type etl) (snd fld))))
    | AE_arrow (lhs,fld) ->
            type_expr env (lbl,(AE_dot ((lbl,AE_star lhs),fld)))
    | AE_unop (op,lexpr) ->
    let (etl,tel)= type_expr env lexpr in
     begin
     match op with
       |AU_minus|AU_plus->
              if compatible etl ET_int then
              (ET_int,TE_unop(op,(etl,tel)))
              else raise(Typing_error (lbl,
                          "type `"^(string_of_type etl)^
                          "' is not compatible with int"))
       |AU_not->if is_num etl then (ET_int,TE_unop(op,(etl,tel)))
               else raise(Typing_error (lbl,
                          Printf.sprintf "type `%s' is not numeric"
                            (string_of_type etl) ))
       |AU_addr->if is_lvalue (snd lexpr) then
                let (etl,tel)= type_expr env lexpr in
                (ET_star(etl),TE_unop(op,(etl,tel)))
               else raise(Typing_error (lbl,
                          "Operand & requires a left value"))

     end
    | AE_binop (op,exp1,exp2) ->
     let (etl1,tel1)= type_expr env exp1 in
     let (etl2,tel2)= type_expr env exp2 in
     begin
      match op with
        |AB_equal|AB_diff|AB_lt|AB_leq|AB_gt|AB_geq ->
          if (is_num etl1 && compatible etl1 etl2) then
          (ET_int,TE_binop(op,(etl1,tel1),(etl2,tel2)))
          else raise(Typing_error (lbl,
                          Printf.sprintf "Numeric expression required"
                            ))
        |AB_plus->
        begin
          match (etl1,etl2) with
            |ET_star(a),_->
                  if compatible etl2 ET_int then
                  (ET_star(a),TE_binop(op,(etl1,tel1),(etl2,tel2)))
                  else raise(Typing_error (lbl,
                          Printf.sprintf "Invalid pointer arithmetic"
                            ))

            |_,ET_star(a)->
            if compatible etl1 ET_int then
                  (ET_star(a),TE_binop(op,(etl1,tel1),(etl2,tel2)))
                  else raise(Typing_error (lbl,
                          Printf.sprintf "Invalid pointer arithmetic"
                            ))
              
            |_,_-> if (compatible etl1 ET_int)&&(compatible etl1 etl2)
                  then (ET_int,TE_binop(op,(etl1,tel1),(etl2,tel2)))
                  else raise(Typing_error (lbl,
                          Printf.sprintf "Operator + requires operands "
                                ^"compatible with int"
                            ))

        end
        | AB_minus->(ET_int,TE_binop(op,(etl1,tel1),(etl2,tel2)))
        | AB_times|AB_div|AB_mod|AB_and|AB_or->
          if (compatible etl1 ET_int)&&(compatible etl1 etl2) then
          (ET_int, TE_binop(op,(etl1,tel1),(etl2,tel2)))
          else raise(Typing_error (lbl,
                          Printf.sprintf "Operators *,/,mod,&&,|| require"^
                          " types compatible with int"))
     end
    | AE_incr(inc,lexpr)->
        let (etl,tel)= type_expr env lexpr in
        if (is_num etl)&& (is_lvalue (snd lexpr)) then 
            (etl,TE_incr(inc,(etl,tel)))
        else raise(Typing_error (lbl,
             Printf.sprintf "Incrementation requires a numeric"
			^ " expression"))
    | AE_call ((_,name),args) ->
            (try
                let proto = Env.find name !proto_env in
                let (_,argument_exprs) = List.fold_left2
                    (fun (argnum,aexprs) value expected_type ->
                     let (actual_type,expr) = type_expr env value in
                     if not (compatible actual_type expected_type) then
                         raise (Typing_error (fst value,
                         ("Function `"^name^"' expects an argument of type `"^
                         (string_of_type expected_type)^
                         (Printf.sprintf "' as argument n°%d." argnum))));
                     (argnum+1, (actual_type,expr)::aexprs))
                    (1,[]) args proto.args in
                (proto.return, TE_call (name, (List.rev argument_exprs)))
            with Not_found ->
                raise (Typing_error (lbl,"Unknown function `"^name^"'"))
               | Invalid_argument _ ->
                raise (Typing_error (lbl,"Too much or not enough arguments "^
                                    "provided for function `"^name^"'")))
(*TODO -> SIZEOF*)
    | _ -> assert(false)


    (* Renvoie le type représenté par l'expression de type correspondante
*)
let rec type_type x = match x with
    | A_void -> ET_void
    | A_int -> ET_int
    | A_char -> ET_char
    (* TODO : check that s is a valid identifier *)
    | A_struct s -> ET_struct (snd s)
    | A_union s -> ET_union (snd s)

(** AJOUT D'IDENTIFIEURS À L'ENVIRONNEMENT *)

    (* Renvoie le type et l'identifiant de la variable déclarée *)
let rec type_and_id_of_avar basetype = function
    | AV_ident (_,s) -> (type_type basetype, s)
    | AV_star x -> let (t,s) = (type_and_id_of_avar basetype x) in
                    (ET_star t, s)

let add_avar_to basetype env v =
    let (value,key) = type_and_id_of_avar basetype v in
    Env.add key value env

    (* Met à jour l'environnement après déclaration de variables *)
                    (* TODO : remove ldecl_vars *)
let type_declvar env (lb,((lbl,basetype),lst)) =
    let foldit (env,l) v =
        (add_avar_to basetype env v, v::l)
    in
    let nouvel_env,decls =  List.fold_left foldit (env,[]) lst in
    (nouvel_env, ((type_type basetype),decls))

let type_arguments env (lb,((lbl,basetype),var)) =
    add_avar_to basetype env var

(** VÉRIFICATION DES INSTRUCTIONS, DÉCLARATIONS **)

    (* Vérifie le type des composants de l'instruction. *)
let rec type_instr returntype env (lbl,instr) = match instr with
    | AI_none -> VT_none
    | AI_inst x -> let a = type_expr env (lbl,x) in VT_inst(a)
    | AI_return(Some x) ->
            let (rettype,retexpr) = (type_expr env x) in
            if not (compatible returntype rettype) then
                raise (Typing_error (lbl,"expected `"^(string_of_type
                returntype)^
                "' as return type"));
            VT_return(Some (rettype,retexpr))
    | AI_return(None) ->
            if ET_void <> returntype then
                raise (Typing_error (lbl,"expected `"^(string_of_type
                returntype)^"' as return type"));
            VT_return(None)
    | AI_if(lexpr,linst)->
    	let (etl,tel)=type_expr env lexpr in
    	if not(is_num etl) then 
       raise(Typing_error (lbl,
      	                  "Numeric expression excepted in"^
                          " conditions"));
       VT_if((etl,tel),type_instr returntype env linst) 
    | AI_if_else(lexpr,linst1,linst2)->
        let (etl,tel)=type_expr env lexpr in
        if not(is_num etl) then
            raise(Typing_error (lbl,
      	                  "Numeric expression excepted in"
                          ^" conditions"));
       VT_if_else((etl,tel),type_instr returntype env linst1,
                    type_instr returntype env linst2) 
    | AI_while(lexpr,linstr)->
  	    let (etl,tel)=type_expr env lexpr in
    	if not(is_num etl) then 
            raise(Typing_error (lbl,
      	                  "Numeric expression excepted in"
                          ^" conditions"));
       VT_while((etl,tel),type_instr returntype env linstr) 
    | AI_for(listexpr1,Some(exproption),listexpr2,instr)->
     let (etl,tel)=type_expr env exproption in
      if (is_num etl) then
         let l1= (List.map (type_expr env) listexpr1) in 
         let l2= (List.map (type_expr env) listexpr2) in
         VT_for(l1,Some (etl,tel),l2,type_instr returntype env instr)     
      else 
        raise(Typing_error (lbl,
      	                  "Numeric excepted."
                          ));

    | AI_bloc(bloc) -> type_bloc returntype env bloc
    | _ -> assert(false)

and type_bloc ret env (dvars,dinstr) =
        let (nenv,lst_wdecl_vars) = update_env type_declvar env dvars in
        let lst_winstr = type_list (type_instr ret) nenv dinstr in
        VT_bloc(lst_wdecl_vars,lst_winstr)

    (* Met à jour l'environnement sans renvoyer de type,
* mais en vérifiant que tout est bien typé *)
let type_decl (lbl,decl) = match decl with
    (* TODO *)
    | Adecl_vars decl ->
            globals_env := (fst (type_declvar !globals_env (lbl,decl)))
            
    | Adecl_fct (ret, nbs, name, args, body) ->
            let rettype = type_type (snd ret) in
            let env = !globals_env in

            (* ajouter les variables à nenv *)
            let (nenv,argslist) = List.fold_left
            (fun (env,lst) (_,((_,t),v)) ->
             let (tt,id) = (type_and_id_of_avar t v) in
             (Env.add id tt env, tt::lst))
            (env,[]) args in

            (* construire le prototype *)
            (* TODO : ajouter les étoiles ! *)
            (* TODO : check that this name is free *)
            proto_env := Env.add (snd name)
            {return = rettype; name = (snd name); args = argslist} !proto_env;
            let _ = type_bloc rettype nenv (snd body) in ()
    | Adecl_typ (is_union, (lbl2,name), decls) ->
            if Env.mem name !sig_env then
                (raise (Typing_error (lbl2,
                Printf.sprintf "type `%s' defined twice" name)));
            let listvars = List.fold_left
                (fun accu (_,((_,a),lst)) ->
                    List.fold_left (fun accu2 h ->
                        (type_and_id_of_avar a h)::accu2)
                    accu lst)
                [] decls in
            let typedef = (if is_union then
                            UnionSig listvars
                           else StructSig listvars) in
            sig_env := Env.add name typedef !sig_env

let type_ast ast =
    List.iter type_decl ast

