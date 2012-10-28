
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
        let _ = Parser.fichier  Lexer.token lexbuf in
        print_string "Successfully parsed.\n";
    with Lexer.Eof -> ()
       | Sys_error _ -> Printf.printf "Unable to open the file %s.\n" filename; exit 2

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
                  parse_file h;
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


