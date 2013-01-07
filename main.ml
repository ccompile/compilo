
(* Lecture des arguments passés en ligne de commande *)

open Errors

let usage = Printf.sprintf
  "Usage: %s source.c"
  (Filename.basename Sys.argv.(0))

let parse_only = ref false
let type_only = ref false
let htmlt= ref false
let htmlp= ref false

let optlist = [
  ("-parse-only", Arg.Unit (fun () -> parse_only := true),
    "\tStop after the parsing step");
  ("-type-only", Arg.Unit (fun () -> type_only := true),
    "\tStop after the typing step");
  ("-htmlp", Arg.Unit (fun ()-> htmlp:= true),
    "\tGenerate an HTML file with the formated source code and the labels");
  ("-htmlt", Arg.Unit (fun ()-> htmlt:= true),
    "\tGenerate an HTML file with the formated source code and the types");
  ("-lisp-mode", Arg.Unit (fun () -> Gen_html.lisp_mode := true),
    "\tPrint lots of parentheses in HTML outputs")
]

let parse_file filename =
  try
    let in_file = open_in filename in
    let lexbuf = Lexing.from_channel in_file in
    lexbuf.Lexing.lex_curr_p <-
      {lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };

        let error_position () =
          string_of_label (make_label lexbuf.Lexing.lex_start_p
            lexbuf.Lexing.lex_curr_p)
        in

        try
          Parser.lfichier  Lexer.token lexbuf
        with Parser.Error ->
          Printf.eprintf "%sError: syntax error\n" (error_position ()); exit 1
            | Lexer.Lexing_error s ->
              Printf.eprintf "%sError: %s\n" (error_position ()) s; exit 1

            with Sys_error _ -> Printf.printf "Unable to open the file %s.\n" filename; exit 2

let rec temp_fun = function
  | [] -> ()
  | (Kildall.Glob _) ::t-> temp_fun t
  | Kildall.Fct(name,nbargs,g,start,locals,liveness)::t ->
          let cl = Irc.allocate_registers g liveness in
          Format.printf "Coloring for %s:\n" name;
          Irc.print_coloring Format.std_formatter cl;
          temp_fun t

let run_compiler filename =
  let ast = parse_file filename in
  if !htmlp then 
    begin
      let htmlout_fname =
        (String.sub filename 0 (String.length filename - 2))
        ^ ".syntax.html" in
      let htmlout = Format.formatter_of_out_channel
        (try
          open_out htmlout_fname
        with Sys_error _ -> stdout)
      in
      Print_ast.print_source htmlout (snd ast) filename
    end;
   if not !parse_only then
   begin
       let typed_tree =
           try
               Type_checker.type_ast ast
           with (Typing_error (pos,reason))->
             Printf.eprintf "%sError: %s\n" (string_of_label pos) reason; exit 1
       in
       if !htmlt then
          begin
            let htmlout_fname =
              (String.sub filename 0 (String.length filename - 2))
              ^ ".types.html" in
            let htmlout = Format.formatter_of_out_channel
              (try
                open_out htmlout_fname
               with Sys_error _ -> stdout)
            in
            Print_typed_ast.print_source htmlout typed_tree filename
        end;
        if not !type_only then
        begin
            let rtl = Rtl.compile_fichier typed_tree in
            let ertl = Ertl.compile_fichier rtl in
            Format.printf "ERTL :@\n@\n";
            Print_ertl.print_ertl Format.std_formatter ertl;
            let ertl_with_uses = Kildall.compute_uses ertl in
            let ltl = Ltl.compute_uses ertl_with_uses in
            Print_ltl.print_ltl Format.std_formatter ltl
        end
   end;
  exit 0

let main () = 
  let args = ref [] in
  let collect arg =
    args := !args @ [arg] in
  Arg.parse optlist collect usage;

  (match !args with
  | [] -> print_string "No source file specified. See -help.\n"
  | h::t -> run_compiler h)
 

let () = main ()


