
(* Lecture des arguments passés en ligne de commande *)

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
        Parser.fichier  Lexer.token lexbuf
    with Sys_error _ -> Printf.printf "Unable to open the file %s.\n" filename; exit 2

let main () = 
    let args = ref [] in
    let collect arg =
        args := !args @ [arg] in
    Arg.parse optlist collect usage;

    (match !args with
    | [] -> print_string "No source file specified. See -help.\n"
    | h::t -> (* TODO (delete the 6 next lines) *)
              if !parse_only then
              begin
                  let ast = parse_file h in
                  let htmlout_fname =
                      (String.sub h 0 (String.length h - 2))
                      ^ ".html" in
                  let htmlout = Format.formatter_of_out_channel
                      (try
                          open_out htmlout_fname
                      with Sys_error _ -> stdout)
                  in
                  Print_ast.print_source htmlout ast h;
                  exit 0
              end
              else if !type_only then
              begin
                  Printf.printf "Typing %s : not implemented.\n" h;
                  exit 2
              end
              else
              begin
                  Printf.printf "Compiling %s : not implemented.\n" h;
                  exit 2
              end)


let () = main ()


