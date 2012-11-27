
(* Préfixes et suffixes de la sortie HTML *)
let html_prefix = "<!DOCTYPE html>\n<html>\n<head>\n<title>Parsing output</title>" ^
  "<style>\n" ^
  ".c_type { color: red; }\n" ^
  ".c_type:hover { text-decoration: underline; }\n" ^
  ".c_ident { color: green; }\n" ^
  ".c_funname { color: blue; }\n" ^
  ".c_ident:hover { text-decoration: underline; }\n" ^
  ".c_keyword { font-weight: bold; }\n" ^
  ".c_cst { color: purple; }\n" ^
  ".c_cst:hover { text-decoration: underline; }\n" ^
  ".token:hover { text-decoration: underline; }\n" ^
  "</style>\n" ^
  "</head>\n<body>"
let h3_input_file_pre = "<h3>Input file:</h3>\n<pre>"
let html_infix =  "</pre>\n<h3>Output:</h3>\n"
let html_suffix = "</body>\n</html>"

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

let rec p_list_scnl printer f = function
  | [] -> ()
  | [a] -> printer f a
  | h::t -> Format.fprintf f "%a;@\n%a" printer h (p_list_scnl printer) t

