
(* Lecture des arguments passés en ligne de commande *)

open Linearize
open Errors

let usage = Printf.sprintf
  "Usage: %s source.c"
  (Filename.basename Sys.argv.(0))

let parse_only = ref false
let type_only = ref false
let htmlt= ref false
let htmlp= ref false
let print_rtl = ref false
let print_ertl = ref false
let print_uses = ref false
let print_ltl = ref false

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
    "\tPrint lots of parentheses in HTML outputs");
  ("-rtl", Arg.Unit (fun () -> print_rtl := true),
    "\tPrint the code at the RTL stage");
  ("-ertl", Arg.Unit (fun () -> print_ertl := true),
    "\tPrint the code at the ERTL stage");
  ("-uses", Arg.Unit (fun () -> print_uses := true),
    "\tPrint the CFG analysis' output");
  ("-ltl", Arg.Unit (fun () -> print_ltl := true),
    "\tPrint the code at the LTL stage");
  ("-graph", Arg.Unit (fun () -> Irc.print_graph_dot := true),
    "\tPrint the interference graph (on stderr, in DOT format)")
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
            let fmt = Format.std_formatter in
            let rtl = Rtl.compile_fichier typed_tree in
            if !print_rtl then
                Print_rtl.p_decl_list fmt rtl;
            let ertl = Ertl.compile_fichier rtl in
            if !print_ertl then
                Print_ertl.print_ertl fmt ertl;
            let ertl_with_uses = Kildall.compute_uses ertl in
            if !print_uses then
                Print_ertl.with_uses fmt ertl_with_uses;
            let ltl = Ltl.compile_fichier ertl_with_uses in
            if !print_ltl then
                Print_ltl.print_ltl fmt ltl;
            Linearize.compile_fichier fmt ltl
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


