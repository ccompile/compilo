
open Ast

(* Module permettant d'afficher un source parsé au format HTML           *)
(* En survolant les lexèmes, on lit leur position dans le fichier source *)

(* Préfixes et suffixes de la sortie HTML *)
let html_prefix = "<!DOCTYPE html>\n<html>\n<head>\n<title>Parsing output</title>" ^
                  "<style>\n" ^
                  ".c_type { color: red; }\n" ^
                  ".c_ident { color: green; }\n" ^
                  ".c_keyword { font-weight: bold; }\n" ^
                  ".c_cst { color: purple; }\n" ^
                  "</style>\n" ^
                  "</head><body>"
let html_suffix = "</body>\n</html>"

let p_label f x =
    Format.fprintf f "File %s, line %d, characters %d-%d"
    x.file x.line x.cbegin x.cend

(* Crée un printer pour le type (label * 'a)
 * à partir d'un printer pour le type 'a *)    
let p_labeled printer f (lbl,x) =
    Format.fprintf f "<span title=\"%a\">%a</span>" p_label lbl printer x

(* Crée un printer pour le type 'a list
 * à partir d'un printer pour le type 'a
 * `sep` est le séparateur *)
let rec p_list sep printer f = function
    | [] -> ()
    | [a] -> printer f a
    | h::t -> Format.fprintf f "%a%s%a" printer h sep (p_list sep printer) t

(* Même chose, mais avec "@\n" comme séparateur.
 * On ne peut pas faire p_list "@\n" parce que le caractère
 * n'est plus interprété par Format (il passe dans un %s) *)
let rec p_list_nl printer f = function
    | [] -> ()
    | [a] -> printer f a
    | h::t -> Format.fprintf f "%a@\n%a" printer h (p_list_nl printer) t

let p_ident f = Format.fprintf f "<span class=\"c_ident\">%s</span>"

let p_lident = p_labeled p_ident

let p_atype f x =
    Format.fprintf f "<span class=\"c_type\">%s</span>"
    (match x with
     | A_void -> "void"
     | A_int -> "int"
     | A_char -> "char"
     | A_struct (_,s) -> Printf.sprintf "<span class=\"c_keyword\">struct</span> %s" s
     | A_union (_,s) -> Printf.sprintf "<span class=\"c_keyword\">union</span> %s" s)

let p_ltype = p_labeled p_atype

let rec p_avar f = function
    | AV_ident li -> p_lident f li
    | AV_star s -> Format.printf "*%a" p_avar s

let p_adecl_vars f (t,lst) =
    Format.fprintf f "%a %a" p_ltype t (p_list ", " p_avar) lst

let p_ldecl_vars = p_labeled p_adecl_vars

let p_stars f nb = Format.fprintf f "%s" (String.make nb '*')

let rec p_incr f = function
    | (IncrRet,i) -> Format.fprintf f "++%a" p_expr (snd i)
    | (DecrRet,i) -> Format.fprintf f "--%a" p_expr (snd i)
    | (RetIncr,i) -> Format.fprintf f "%a++" p_expr (snd i)
    | (RetDecr,i) -> Format.fprintf f "%a--" p_expr (snd i)

and p_unop f = function
    | (AU_addr,i)  -> Format.fprintf f "&(%a)" p_expr (snd i)
    | (AU_not,i)   -> Format.fprintf f "!(%a)" p_expr (snd i)
    | (AU_minus,i) -> Format.fprintf f "- (%a)" p_expr (snd i)
    | (AU_plus,i)  -> Format.fprintf f "+ (%a)" p_expr (snd i)

and p_binop f (op,a,b) =
    let strop = (match op with
    | AB_equal -> "==" 
    | AB_diff  -> "!=" 
    | AB_lt    -> "<" 
    | AB_leq   -> "<=" 
    | AB_gt    -> ">" 
    | AB_geq   -> ">=" 
    | AB_plus  -> "+" 
    | AB_minus -> "-" 
    | AB_times -> "*" 
    | AB_div   -> "/" 
    | AB_mod   -> "%" 
    | AB_and   -> "&&" 
    | AB_or    -> "||")
    in Format.fprintf f "(%a) %s (%a)" p_expr (snd a) strop p_expr (snd b) 

and p_expr f = function
    | AE_int i        -> Format.fprintf f "<span class=\"c_cst\">%d</span>" i
    | AE_str s        -> Format.fprintf f "<span class=\"c_cst\">\"%s\"</span>" s
    | AE_ident li     -> Format.fprintf f "%s" li
    | AE_star s       -> Format.fprintf f "*(%a)" p_expr (snd s)
    | AE_brackets(a,b)-> Format.fprintf f "%a[%a]" p_expr (snd a) p_expr (snd b)
    | AE_dot(a,b)     -> Format.fprintf f "%a.%a" p_expr (snd a) p_expr (snd b)
    | AE_arrow(a,b)   -> Format.fprintf f "%a->%a" p_expr (snd a) p_lident b
    | AE_gets(a,b)    -> Format.fprintf f "%a = %a" p_expr (snd a) p_expr (snd b)
    | AE_call(a,b)    -> Format.fprintf f "%a(%a)" p_lident a
                          (p_list ", " p_lexpr) b
    | AE_incr(a,b)    -> p_incr f (a,b)
    | AE_unop(a,b)    -> p_unop f (a,b)
    | AE_binop(o,a,b) -> p_binop f (o,a,b)
    | AE_sizeof(t,i)  -> Format.fprintf f
                        "<span class=\"c_keyword\">sizeof</span>(%a%a)"
                        p_stars i p_ltype t

and p_lexpr f = p_labeled p_expr f

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
        Format.fprintf f "@\n{@[<hov 4>@\n%a;@\n@\n%a@]@\n}"
        (p_list ";\n" p_ldecl_vars) dv (p_list_nl (p_labeled p_instr)) il 
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
    | Adecl_vars d -> p_adecl_vars f d
    | Adecl_typ (false,i,ld) ->
            Format.fprintf f "struct %a@.{@[<hov 4>%a@]@\n}@\n" p_lident i
            (p_list ";\n" p_ldecl_vars) ld
    | Adecl_typ (true,i,ld) ->
            Format.fprintf f "union %a@.{@[<hov 4>%a@]@\n}@\n" p_lident i
            (p_list ";\n" p_ldecl_vars) ld
    | Adecl_fct (t,n,i,la,b) ->
            Format.fprintf f "%a%a %a(%a)%a@\n"
            p_ltype t
            p_stars n
            p_lident i
            (p_list ", " p_largument) la
            p_bloc (snd b)

let p_ldecl = p_labeled p_adecl

let p_fichier f x =
    Format.fprintf f "%s@.<pre>@\n%a@.</pre>@.%s@."
    html_prefix
    (p_list "\n" p_ldecl) x 
    html_suffix

