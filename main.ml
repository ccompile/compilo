
(* Lecture des arguments passés en ligne de commande *)

open Errors

let usage = Printf.sprintf
"Usage: %s source.c"
(Filename.basename Sys.argv.(0))

let parse_only = ref false
let type_only = ref false

let optlist = [
    ("-parse-only", Arg.Unit (fun () -> parse_only := true),
     "\tstop after the parsing step");
    ("-type-only", Arg.Unit (fun () -> type_only := true),
     "\tstop after the typing step");
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
            Parser.fichier  Lexer.token lexbuf
        with Parser.Error ->
            Printf.printf "%sError: syntax error\n" (error_position ()); exit 1
           | Lexer.Lexing_error s ->
            Printf.printf "%sError: %s\n" (error_position ()) s; exit 1

    with Sys_error _ -> Printf.printf "Unable to open the file %s.\n" filename; exit 2

   
let run_compiler filename =
    try 
          if !parse_only then
          begin
              let ast = parse_file filename in
              let htmlout_fname =
                  (String.sub filename 0 (String.length filename - 2))
                  ^ ".html" in
              let htmlout = Format.formatter_of_out_channel
                  (try
                      open_out htmlout_fname
                  with Sys_error _ -> stdout)
              in
              Print_ast.print_source htmlout ast filename;
              exit 0
          end
          else if !type_only then
          begin
              Printf.printf "Typing %s : not implemented.\n" filename;
              exit 2
          end
          else
          begin
              Printf.printf "Compiling %s : not implemented.\n" filename;
              exit 2
          end
    with (SyntaxError explanation) ->
        Printf.printf ""

let main () = 
    let args = ref [] in
    let collect arg =
        args := !args @ [arg] in
    Arg.parse optlist collect usage;

    (match !args with
    | [] -> print_string "No source file specified. See -help.\n"
    | h::t -> run_compiler h)
 

let () = main ()


