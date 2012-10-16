{open Lexing

exception Lexing_error of string

let kwd_tbl =
["char",CHAR;"else",ELSE;"for",FOR;"if",IF;"int",INT;"return",RETURN;"sizeof",SIZEOF;"struct",STRUCT;"union",UNION;"void",VOID;"while",WHILE]
 let id_or_kwd s = try List.assoc s kwd_tbl with _-> IDENT s
}

let chiffre = [0-9]
let aplha = [a-z] | [A-Z]
let ident = (alpha | _) (alpha | chiffre | _)*


