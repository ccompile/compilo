open Ast
open Types
open Errors
open Int32


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

    (* Ajoute n étoiles à un type *)
let rec add_stars basetype = function
    | 0 -> basetype
    | n -> ET_star (add_stars basetype (n-1))

    (* Cet identifiant est-il valide pour désigner un struct (resp, un union) ?
     *)
let check_type_name (lbl,id) is_union =
    if (try
        (match Env.find id !sig_env with
          | UnionSig _ -> not is_union
          | StructSig _ -> is_union)
    with Not_found -> true) then
        typing_error lbl ("`"^id^"' is not a valid "^
        (if is_union then "union" else "struct")^" name")

    (* Renvoie le type représenté par l'expression de type correspondante
*)
let rec type_type = function
    | A_void -> ET_void
    | A_int -> ET_int
    | A_char -> ET_char
    | A_struct s -> check_type_name s false;
                    ET_struct (snd s)
    | A_union s -> check_type_name s true;
                   ET_union (snd s)

    (* Calcule sizeof *)
let rec sizeof = function
    | ET_void -> 0
    | ET_null
    | ET_int -> 4
    | ET_char -> 1
    | ET_star _ -> 4
    | ET_struct s
    | ET_union s ->
            try
                (match Env.find s !sig_env with
                 | UnionSig (n,_)
                 | StructSig (n,_) -> n)
            with Not_found -> assert false

(** TYPAGE DES EXPRESSIONS **)

    (* Renvoie l'arbre étiqueté par ses types *)
let rec type_expr env (lbl,expr) = match expr with
    | AE_int (a) when a=of_int 0 -> (ET_null, TE_int (of_int 0))
    | AE_int x -> (ET_int, TE_int x)
    | AE_char c -> (ET_char, TE_char c)
    | AE_str x -> (ET_star (ET_char), TE_str x)
    | AE_ident s ->
            let (lvl,typ) =
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
             | _ -> typing_error lbl ("only pointers can be dereferenced"
             ^", and this value has type `"^(string_of_type et)^"'"))         
    | AE_gets (lhs,rhs) ->
    (*etl : expression type left, tel: typed expression left*)
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
                            | UnionSig (_,lst)
                            | StructSig (_,lst) ->
                                    let ft = get_field_type (snd fld) lst
                                    in
                                    (ft, TE_dot ((etl,tel),snd fld)))
                  | _ -> raise Not_found)
            with Not_found ->
                typing_error lbl 
                (Printf.sprintf "type `%s' has no field named `%s'"
                (string_of_type etl) (snd fld)))
    | AE_arrow (lhs,fld) ->
            type_expr env (lbl,(AE_dot ((lbl,AE_star lhs),fld)))
    | AE_unop (op,lexpr) ->
    let (etl,tel)= type_expr env lexpr in
     begin
     match op with
       |AU_minus|AU_plus->
              if compatible etl ET_int then
              (ET_int,TE_unop(op,(etl,tel)))
              else typing_error lbl ("type `"^(string_of_type etl)^
                          "' is not compatible with int")
       |AU_not->if is_num etl then (ET_int,TE_unop(op,(etl,tel)))
               else typing_error lbl (Printf.sprintf "type `%s' is not numeric"
                            (string_of_type etl))
       |AU_addr->if is_lvalue (snd lexpr) then
                let (etl,tel)= type_expr env lexpr in
                (ET_star(etl),TE_unop(op,(etl,tel)))
               else typing_error lbl ("operand & requires a left value")

     end
    | AE_binop (op,exp1,exp2) ->
     let (etl,tel)= type_expr env exp1 in
     let (etr,ter)= type_expr env exp2 in
     begin
      match op with
        |AB_equal|AB_diff|AB_lt|AB_leq|AB_gt|AB_geq ->
          if (is_num etl && compatible etl etr) then
          (ET_int,TE_binop(op,(etl,tel),(etr,ter)))
          else typing_error lbl (Printf.sprintf "numeric expression required")
        |AB_plus
        | AB_minus ->
        begin
          match (etl,etr) with
            |ET_star(a),_->
                  if compatible etr ET_int then
                  (ET_star(a),TE_binop(op,(etl,tel),(etr,ter)))
                  else typing_error lbl (Printf.sprintf "invalid pointer arithmetic")
            |_,ET_star(a) when op=AB_plus->
 	          if compatible etl ET_int then
                  (ET_star(a),TE_binop(op,(etl,tel),(etr,ter)))
                  else typing_error lbl (Printf.sprintf "invalid pointer arithmetic")   
	    |_,_-> if (compatible etl ET_int)&&(compatible etl etr)
                  then (ET_int,TE_binop(op,(etl,tel),(etr,ter)))
                  else typing_error lbl (Printf.sprintf "operator "^(if op = AB_plus then "+"
                          else "-")^" requires operands "
                                ^"compatible with int")

        end
        | AB_times|AB_div|AB_mod|AB_and|AB_or->
          if (compatible etl ET_int)&&(compatible etl etr) then
          (ET_int, TE_binop(op,(etl,tel),(etr,ter)))
          else typing_error lbl (Printf.sprintf "operators *,/,mod,&&,|| require"^
                          " types compatible with int")
     end
    | AE_incr(inc,lexpr)->
        let (etl,tel)= type_expr env lexpr in
        if (is_num etl)&& (is_lvalue (snd lexpr)) then 
            (etl,TE_incr(inc,(etl,tel)))
        else typing_error lbl (Printf.sprintf "incrementation requires a numeric"
			^ " expression")
    | AE_call ((_,name),args) ->
            (try
                let proto = Env.find name !proto_env in
                let (_,argument_exprs) = List.fold_left2
                    (fun (argnum,aexprs) value expected_type ->
                     let (actual_type,expr) = type_expr env value in
                     if not (compatible actual_type expected_type) then
                         raise (Typing_error (fst value,
                         ("function `"^name^"' expects an argument of type `"^
                         (string_of_type expected_type)^
                         (Printf.sprintf "' as argument n°%d." argnum))));
                     (argnum+1, (actual_type,expr)::aexprs))
                    (1,[]) args proto.args in
                (proto.return, TE_call (name, (List.rev argument_exprs)))
            with Not_found ->
                typing_error lbl ("unknown function `"^name^"'")
               | Invalid_argument _ ->
                typing_error lbl ("too much or not enough arguments "^
                                    "provided for function `"^name^"'"))
    |AE_sizeof(ltype,ent)->
          let et = type_type (snd ltype) in
          (ET_int, TE_int (of_int( sizeof et)))    


(** AJOUT D'IDENTIFIEURS À L'ENVIRONNEMENT *)

    (* Renvoie le type et l'identifiant de la variable déclarée *)
let rec type_and_id_of_avar basetype = function
    | AV_ident s -> (type_type basetype, s)
    | AV_star x -> let (t,s) = (type_and_id_of_avar basetype x) in
                    (ET_star t, s)

let is_bound_with_level lvl id env =
    try
        let (actual_lvl,value) = Env.find id env in
        actual_lvl = lvl
    with Not_found -> false

let add_avar_to lvl basetype env v =
    let (value,(lbl,key)) = type_and_id_of_avar basetype v in
    if (is_bound_with_level lvl key env) ||
       (lvl = 0 && Env.mem key !proto_env) then
        typing_error lbl 
        ("identifier `"^key^"' is already bound");
    Env.add key (lvl,value) env

    (* Met à jour l'environnement après déclaration de variables *)
                    (* TODO : remove ldecl_vars *)
let type_declvar lvl env (lb,((lbl,basetype),lst)) =
    if basetype = A_void then
        typing_error lbl 
        ("variable or field declared void");
    let foldit (env,l) v =
        (* let (_,id) = type_and_id_of_avar basetype v in *)
        (add_avar_to lvl basetype env v, v::l)
    in
    let nouvel_env,decls =  List.fold_left foldit (env,[]) lst in
    (nouvel_env, ((type_type basetype),decls))

let type_arguments env (lb,((lbl,basetype),var)) =
    add_avar_to 1 basetype env var

(** VÉRIFICATION DES INSTRUCTIONS, DÉCLARATIONS **)

    (* Vérifie le type des composants de l'instruction. *)
let rec type_instr returntype lvl env (lbl,instr) = match instr with
    | AI_none -> VT_none
    | AI_inst x -> let a = type_expr env (lbl,x) in VT_inst(a)
    | AI_return(Some x) ->
            let (rettype,retexpr) = (type_expr env x) in
            if not (compatible returntype rettype) then
                typing_error lbl ("expected `"^(string_of_type
                returntype)^
                "' as return type");
            VT_return(Some (rettype,retexpr))
    | AI_return(None) ->
            if ET_void <> returntype then
                typing_error lbl ("expected `"^(string_of_type
                returntype)^"' as return type");
            VT_return(None)
    | AI_if(lexpr,linst)->
    	let (etl,tel)=type_expr env lexpr in
    	if not(is_num etl) then 
       typing_error lbl ("numeric expression excepted in"^
                          " conditions");
       VT_if((etl,tel),type_instr returntype lvl env linst) 
    | AI_if_else(lexpr,linst1,linst2)->
        let (etl,tel)=type_expr env lexpr in
        if not(is_num etl) then
            typing_error lbl ("numeric expression excepted in"
                          ^" conditions");
       VT_if_else((etl,tel),type_instr returntype lvl env linst1,
                    type_instr returntype lvl env linst2) 
    | AI_while(lexpr,linstr)->
  	    let (etl,tel)=type_expr env lexpr in
    	if not(is_num etl) then 
            typing_error lbl ("numeric expression excepted in"
                          ^" conditions");
       VT_while((etl,tel),type_instr returntype lvl env linstr) 
    | AI_for(listexpr1,Some(exproption),listexpr2,instr)->
     let (etl,tel)=type_expr env exproption in
      if (is_num etl) then
         let l1= (List.map (type_expr env) listexpr1) in 
         let l2= (List.map (type_expr env) listexpr2) in
         VT_for(l1,Some (etl,tel),l2,type_instr returntype lvl env instr)     
      else 
        typing_error lbl ("numeric expression excepted as condition.");

    | AI_bloc(bloc) -> VT_bloc(type_bloc returntype (lvl+1) env bloc)
    | _ -> assert(false) (* TODO : remove me ? *)

and type_bloc ret lvl env (dvars,dinstr) =
        let (nenv,lst_wdecl_vars) = update_env (type_declvar lvl) env dvars in
        let lst_winstr = type_list (type_instr ret lvl) nenv dinstr in
        (lst_wdecl_vars,lst_winstr)

let field_is_bound id lst =
    List.exists (fun (tt,name) -> id = name) lst

let check_prototype_of_main_function lbl = function
     | { return = ET_int; args = [] }
     | { return = ET_int; args = [ET_int; ET_star (ET_star ET_char)]} ->()
     | _ -> typing_error lbl ("function `main' has invalid prototype")


    (* Met à jour l'environnement sans renvoyer de type,
* mais en vérifiant que tout est bien typé *)
let type_decl (lbl,decl) = match decl with
    | Adecl_vars decl ->
            let (nouvel_env,d) = (type_declvar 0 (*=level*)
				   !globals_env (lbl,decl)) in
            globals_env := nouvel_env;
	    Tdecl_vars(d)
    | Adecl_fct (ret, nbstars, (_,name), args, body) ->
            let rettype = type_type (snd ret) in
            let env = !globals_env in

            (* ajouter les variables à nenv *)
            let (nenv,args_with_names) = List.fold_left
            (fun (env,lst) (_,((_,t),v)) ->
             let (tt,(_,id)) = (type_and_id_of_avar t v) in
             Env.add id (1,tt) env, (tt,id)::lst)
            (env,[]) args in

	    (* Enlever les noms pour construire le prototype *)
	    let types_list = List.map (fun (a,b) -> a) args_with_names in

            (* construire le prototype *)
            if Env.mem name !proto_env then
                (typing_error lbl (Printf.sprintf
                "the function `%s' has already been defined" name));
            if Env.mem name !globals_env then
                (typing_error lbl (Printf.sprintf
                "the identifier `%s' is already bound." name));

            let rettype_with_stars = add_stars rettype nbstars in
            let proto =  {return = rettype_with_stars; name = name;
                                        args = List.rev types_list} in
            if name = "main" then
                check_prototype_of_main_function lbl proto;

            proto_env := Env.add name proto !proto_env;
            
            let typed_bloc = type_bloc rettype_with_stars 1 (*=lvl*)
		nenv (snd body) in

	    Tdecl_fct (rettype_with_stars, name, List.rev
args_with_names, typed_bloc)
    | Adecl_typ (is_union, (lbl2,name), decls) ->
            if Env.mem name !sig_env then
                typing_error lbl2 (Printf.sprintf "type `%s' defined twice" name);

             (* Ajout d'une signature fantoche pour autoriser les pointeurs sur
              * le type lui-même *)   
            sig_env := Env.add name
                    (if is_union then UnionSig (0,[]) else StructSig (0,[])) !sig_env;
            let self_type = (if is_union then ET_union name else ET_struct name)
            in

            let sum_of_sizes = ref 0 in
            let listvars = List.fold_left
                (fun accu (_,((lbl,var),lst)) ->
                    List.fold_left (fun accu2 h ->
                        let (tt,(_,id)) = (type_and_id_of_avar var h) in
                        if tt = ET_void then
                            typing_error lbl 
                               ("field `"^id^"' declared void");
                        if tt = self_type then
                            typing_error lbl 
                               ("field `"^id^"' has imcomplete type");
                        if field_is_bound id accu2 then
                            typing_error lbl 
                               ("field `"^id^"' declared twice");
                        sum_of_sizes := !sum_of_sizes + sizeof tt;
                        (tt,id)::accu2)
                    accu lst)
                [] decls in
            let typedef = (if is_union then
                            UnionSig (!sum_of_sizes,listvars)
                           else StructSig (!sum_of_sizes,listvars)) in
            sig_env := Env.add name typedef !sig_env;
	    Tdecl_typ(is_union, name, listvars) 

let type_ast (lbl,ast) =
    let fich = List.rev
	(List.fold_left (fun l x -> (type_decl x)::l) [] ast)
    in
    if not (Env.mem "main" !proto_env) then
        typing_error lbl ("no `main' function declared");
    fich    

