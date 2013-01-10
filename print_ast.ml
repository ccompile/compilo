
open Ast
open Gen_html

(* Module permettant d'afficher un source parsé au format HTML           *)
(* En survolant les lexèmes, on lit leur position dans le fichier source *)

let p_label f x =
  Format.fprintf f "File %s, line %d, characters %d-%d"
    x.file x.line x.cbegin x.cend

(* Crée un printer pour le type (label * 'a)
 * à partir d'un printer pour le type 'a *)    
let p_labeled printer f (lbl,x) =
  Format.fprintf f "<span title=\"%a\">%a</span>" p_label lbl printer x

let p_ident f = Format.fprintf f "<span class=\"c_ident\">%s</span>"

let p_lident = p_labeled p_ident

let p_funname f = Format.fprintf f "<span class=\"c_funname\">%s</span>"

let p_lfunname = p_labeled p_funname

let p_atype f x =
  Format.fprintf f "<span class=\"c_type\">%s</span>"
    (match x with
    | A_void -> "void"
    | A_int -> "int"
    | A_char -> "char"
    | A_struct (_,s) ->
      Printf.sprintf "<span class=\"c_keyword\">struct</span> %s" s
    | A_union (_,s) ->
      Printf.sprintf "<span class=\"c_keyword\">union</span> %s" s)

let p_ltype = p_labeled p_atype

let rec p_avar f = function
  | AV_ident li -> p_lident f li
  | AV_star s -> Format.fprintf f "*%a" p_avar s

let p_adecl_vars f (t,lst) =
  Format.fprintf f "%a %a" p_ltype t (p_list ", " p_avar) lst

let p_ldecl_vars = p_labeled p_adecl_vars

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
  Format.fprintf f "%a %s %a"
    p_pexpr (snd a)
    (Gen_html.strop op)
    p_pexpr (snd b) 

and p_expr f = function
  | AE_int i      -> Format.fprintf f "<span class=\"c_cst\">%ld</span>" i
  | AE_char c     -> Format.fprintf f "<span class=\"c_cst\">'%c'</span>" c
  | AE_str s      -> Format.fprintf f "<span class=\"c_cst\">\"%s\"</span>" s
  | AE_ident li     -> Format.fprintf f "%a" p_ident li
  | AE_star s       -> Format.fprintf f "*(%a)" p_lexpr s
  | AE_brackets(a,b)-> Format.fprintf f "%a[%a]" p_lexpr a p_lexpr b
  | AE_dot(a,b)     -> Format.fprintf f "%a.%a" p_lexpr a p_lident b
  | AE_arrow(a,b)   -> Format.fprintf f "%a->%a" p_lexpr a p_lident b
  | AE_call(a,b)    -> Format.fprintf f "%a(%a)" p_lfunname a
    (p_list ", " p_lexpr) b
  | AE_incr(a,b)    -> p_par p_incr f (a,b)
  | AE_unop(a,b)    -> p_unop f (a,b)
  | AE_binop(o,a,b) -> p_binop f (o,a,b)
  | AE_sizeof(t,i)  -> Format.fprintf f
    "<span class=\"c_keyword\">sizeof</span>(%a%a)"
    p_stars i p_ltype t

and p_lexpr f = p_labeled p_expr f
and p_pexpr f = p_par p_expr f

let rec p_list_lexpr = p_list ", " p_lexpr

and p_opt_lexpr f = function
  | None -> ()
  | Some x -> p_lexpr f x

and p_instr f = function
  | AI_none -> Format.fprintf f ";"
  | AI_inst e -> Format.fprintf f "%a;" p_expr e
  | AI_if (c,i) -> Format.fprintf f
    "<span class=\"c_keyword\">if</span>(%a)%a"
    p_lexpr c p_in_bloc i
  | AI_if_else (c,i1,i2) -> Format.fprintf f
  "<span class=\"c_keyword\">if</span>(%a)%a<span class=\"c_keyword\">else@\n</span>%a"
    p_lexpr c p_in_bloc i1 p_in_bloc i2
  | AI_while (c,i) -> Format.fprintf f
    "<span class=\"c_keyword\">while</span>(%a)%a"
    p_lexpr c p_in_bloc i
  | AI_for (a1,a2,a3,b) ->Format.fprintf f
    "<span class=\"c_keyword\">for</span>(%a; %a; %a)%a"
    p_list_lexpr a1
    p_opt_lexpr a2
    p_list_lexpr a3
    p_in_bloc b
  | AI_bloc x -> p_bloc f x  
  | AI_return x -> Format.fprintf f
    "<span class=\"c_keyword\">return</span> %a;"
    p_opt_lexpr x

and p_bloc f (dv,il) =
  if dv <> [] then
    Format.fprintf f "@\n{@[<hov 4>@\n%a;@\n@\n%a@]@\n}@\n"
      (p_list_scnl p_ldecl_vars) dv (p_list_nl (p_labeled p_instr)) il 
  else
    Format.fprintf f "@\n{@[<hov 4>@\n%a@]@\n}"
      (p_list_nl (p_labeled p_instr)) il 

(* Fonction qui force l'indentation dans des codes comme celui-là :
 * if(c)
 *      do_something();
 *)
and p_in_bloc f = function
  | (lbl,AI_bloc x) -> p_bloc f x
  | (lbl,y) -> Format.fprintf f "@[<hov 4>    %a@]@\n" p_instr y

let p_aargument f (t,v) = Format.fprintf f "%a %a" p_ltype t p_avar v
let p_largument = p_labeled p_aargument 

let p_adecl f = function
  | Adecl_vars d -> Format.fprintf f "%a;@\n" p_adecl_vars d
  | Adecl_typ (false,i,ld) ->
    Format.fprintf f "struct %a@.{@[<hov 4>@\n%a;@]@\n};@\n" p_lident i
      (p_list_scnl p_ldecl_vars) ld
  | Adecl_typ (true,i,ld) ->
    Format.fprintf f "union %a@.{@[<hov 4>@\n%a;@]@\n};@\n" p_lident i
      (p_list_scnl p_ldecl_vars) ld
  | Adecl_fct (t,n,i,la,b) ->
    Format.fprintf f "%a%a %a(%a)%a@\n"
      p_ltype t
      p_stars n
      p_lfunname i
      (p_list ", " p_largument) la
      p_bloc (snd b)

let p_ldecl = p_labeled p_adecl

let p_fichier f x =
  Format.fprintf f "@.<pre>@\n%a@.</pre>@.@."
    (p_list "\n" p_ldecl) x 

let rec read_file accu istream =
  try
    read_file (accu ^ "\n" ^ (input_line istream)) istream
  with End_of_file -> accu

let print_source f ast filename =
  let in_file = open_in filename in
  let source = read_file "" in_file in

  Format.fprintf f "%s@.%s@.%s@.%s@.%a@.%s@."
    html_prefix
    h3_input_file_pre
    source
    html_infix
    p_fichier ast
    html_suffix


