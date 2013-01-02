
open Ast
open Gen_html
open Types
open Type_checker

(* Affiche un expr_type *)
let p_ctype f x =
  Format.fprintf f "%s" (string_of_type x)

(* Affiche un prototype *)
let p_proto f x =
  Format.fprintf f "%a %s(%a)" p_ctype x.return x.name
    (p_list ", " p_ctype) x.args ;;

(* Crée un printer pour le type (expr_type * 'a)
 * à partir d'un printer pour le type 'a *)    
let p_typed printer f (typ,x) =
  Format.fprintf f "<span title=\"%a\">%a</span>" p_ctype typ
    printer x

let p_with_proto printer f (proto,x) =
  Format.fprintf f "<span title=\"%a\">%a</span>" p_proto proto
    printer x

let p_ident f = Format.fprintf f "<span class=\"c_ident\">%s</span>"

let p_tident = p_typed p_ident

let p_field f (t,n) =
  Format.fprintf f "%s %s" (string_of_type t) n

let p_funname f name = Format.fprintf f
  "<span class=\"c_funname\" title=\"%a\">%s</span>"
  p_proto (Env.find name !proto_env) name

let p_ctype f x =
  let rec p_f_type f = function
    | ET_null -> Format.fprintf f "nulltype"
    | ET_void -> Format.fprintf f "void"
    | ET_int -> Format.fprintf f "int"
    | ET_char -> Format.fprintf f "char"
    | ET_star e -> Format.fprintf f "*%a" p_f_type e
    | ET_struct s -> Format.fprintf f "<span class=\"c_keyword\">struct</span> %s" s
    | ET_union s -> Format.fprintf f "<span class=\"c_keyword\">union</span> %s" s
  in
  Format.fprintf f "<span class=\"c_type\">%a</span>" p_f_type x     


let rec p_avar f = function
  | AV_ident li -> p_ident f (snd li)
  | AV_star s -> Format.fprintf f "*%a" p_avar s

let p_tdecl_vars = p_list_scnl p_field

let p_stars f nb = Format.fprintf f "%s" (String.make nb '*')

let rec p_incr f = function
  | (IncrRet,i) -> Format.fprintf f "++%a" p_pexpr (snd i)
  | (DecrRet,i) -> Format.fprintf f "--%a" p_pexpr (snd i)
  | (RetIncr,i) -> Format.fprintf f "%a++" p_pexpr (snd i)
  | (RetDecr,i) -> Format.fprintf f "%a--" p_pexpr (snd i)

and p_unop f = function
  | (AU_addr,i)  -> Format.fprintf f "&%a" p_pexpr (snd i)
  | (AU_not,i)   -> Format.fprintf f "!%a" p_pexpr (snd i)
  | (AU_minus,i) -> Format.fprintf f "- %a" p_pexpr (snd i)
  | (AU_plus,i)  -> Format.fprintf f "+ %a" p_pexpr (snd i)

and p_binop f (op,a,b) =
  Format.fprintf f "%a %s %a" p_ptexpr a (Gen_html.strop op)  p_ptexpr b

and p_expr f = function
  | TE_int i        -> Format.fprintf f "<span class=\"c_cst\">%s</span>"
    (Int32.to_string i)
  | TE_char c       -> Format.fprintf f "<span class=\"c_cst\">'%c'</span>" c
  | TE_str s        -> Format.fprintf f "<span class=\"c_cst\">\"%s\"</span>" s
  | TE_ident li     -> Format.fprintf f "%a" p_ident li
  | TE_star s       -> Format.fprintf f "*(%a)" p_texpr s
  | TE_dot(a,b)     -> Format.fprintf f "%a.%a" p_texpr a p_ident b
  | TE_gets(a,b)    -> Format.fprintf f "%a = %a" p_texpr a p_texpr b
  | TE_call(a,b)    -> Format.fprintf f "%a(%a)" p_funname a
    (p_list ", " p_texpr) b
  | TE_incr(a,b)    -> p_incr f (a,b)
  | TE_unop(a,b)    -> p_unop f (a,b)
  | TE_binop(o,a,b) -> p_binop f (o,a,b)
and p_texpr f = p_typed p_expr f
and p_pexpr f = p_par p_expr f
and p_ptexpr f = p_par p_texpr f

let rec p_list_texpr = p_list ", " p_texpr

and p_opt_texpr f = function
  | None -> ()
  | Some x -> p_texpr f x

and p_instr f = function
  | VT_none -> Format.fprintf f ";"
  | VT_inst e -> Format.fprintf f "%a;" p_texpr e
  | VT_if (c,i) -> Format.fprintf f
    "<span class=\"c_keyword\">if</span>(%a)%a"
    p_texpr c p_in_bloc i
  | VT_if_else (c,i1,i2) -> Format.fprintf f
    "<span class=\"c_keyword\">if</span>(%a)%a<span class=\"c_keyword\">else@\n</span>%a"
    p_texpr c p_in_bloc i1 p_in_bloc i2
  | VT_while (c,i) -> Format.fprintf f
    "<span class=\"c_keyword\">while</span>(%a)%a"
    p_texpr c p_in_bloc i
  | VT_for (a1,a2,a3,b) ->Format.fprintf f
    "<span class=\"c_keyword\">for</span>(%a; %a; %a)%a"
    p_list_texpr a1
    p_opt_texpr a2
    p_list_texpr a3
    p_in_bloc b
  | VT_bloc x -> p_bloc f x  
  | VT_return x -> Format.fprintf f
    "<span class=\"c_keyword\">return</span> %a;"
    p_opt_texpr x

and p_bloc f (dv,il) =
  if dv <> [] then
    Format.fprintf f "@\n{@[<hov 4>@\n%a;@\n@\n%a@]@\n}@\n"
     p_tdecl_vars dv (p_list_nl (p_instr)) il 
  else
    Format.fprintf f "@\n{@[<hov 4>@\n%a@]@\n}"
      (p_list_nl p_instr) il 

(* Fonction qui force l'indentation dans des codes comme celui-là :
 * if(c)
 *      do_something();
 *)
and p_in_bloc f = function
  | VT_bloc x -> p_bloc f x
  | y -> Format.fprintf f "@[<hov 4>    %a@]@\n" p_instr y

let p_wargument f (t,v) = Format.fprintf f "%a %a" p_ctype t p_avar v

let p_wdecl f = function
  | Tdecl_vars d -> Format.fprintf f "%a;@\n" p_tdecl_vars d
  | Tdecl_typ (false,i,ld) ->
    Format.fprintf f "struct %a@.{@[<hov 4>@\n%a;@]@\n};@\n" p_ident i
      (p_list_scnl p_field) ld
  | Tdecl_typ (true,i,ld) ->
    Format.fprintf f "union %a@.{@[<hov 4>@\n%a;@]@\n};@\n" p_ident i
      (p_list_scnl p_field) ld
  | Tdecl_fct (t,i,la,b) ->
    Format.fprintf f "%a %a(%a)%a@\n"
      p_ctype t
      p_funname i
      (p_list ", " p_field) la
      p_bloc b

let p_fichier f x =
  Format.fprintf f "@.<pre>@\n%a@.</pre>@.@."
    (p_list "\n" p_wdecl) x 

let rec read_file accu istream =
  try
    read_file (accu ^ "\n" ^ (input_line istream)) istream
  with End_of_file -> accu

let print_source f ast filename =
  Format.fprintf f "%s@.<h3>%s</h3>@.%a@.%s@."
    html_prefix
    filename
    p_fichier ast
    html_suffix


