{open Lexing

exception Lexing_error of string

let kwd_tbl =
["char",CHAR;"else",ELSE;"for",FOR;"if",IF;"int",INT;"return",RETURN;"sizeof",SIZEOF;"struct",STRUCT;"union",UNION;"void",VOID;"while",WHILE]

}
