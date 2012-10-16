{open Lexing

exception Lexing_error of string

let kwd_tbl =
["char",CHAR;"else",ELSE;"for",FOR;"if",IF;"int",INT;"return",RETURN;"sizeof",SIZEOF;"struct",STRUCT;"union",UNION;"void",VOID;"while",WHILE]

}

let chiffre = [0-9]
let aplha = [a-z] | [A-Z]
let ident = (alpha | _) (alpha | chiffre | _)*


