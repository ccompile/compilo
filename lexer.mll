{
  open Lexing
  open Ast
  open Parser
  open Errors

  exception Lexing_error of string 
  exception Eof
  let kwd_tbl =
  	["char",CHAR;"else",ELSE;"for",FOR;"if",IF;"int",INT;"return",RETURN;
	  "sizeof",SIZEOF;"struct",STRUCT;"union",UNION;"void",VOID;"while",WHILE]
  let id_or_kwd s = try List.assoc s kwd_tbl with _-> IDENT s

  let localstring=ref ""

  let newline lexbuf =
    let pos= lexbuf.lex_curr_p in
    lexbuf.lex_curr_p<-{ pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }  

  let pr = Printf.printf "%s\n"
 
let asci2int = function
  |'a'|'A'-> 10
  |'b'|'B'-> 11
  |'c'|'C'-> 12
  |'d'|'D'-> 13
  |'e'|'E'-> 14
  |'f'|'F'-> 15
  |_ as c-> int_of_char(c)-48 
 
let int_of_octhexstring base st = 
  let s= String.length st in
  let puis = ref 1 in
  let res = ref 0 in
  for i=s-1 downto 0 do
    res := !res + (asci2int st.[i]) * !puis;
    puis := base * !puis;
  done;
  !res

}


let chiffre = ['0'-'9']
let alpha = ['a'-'z'] | ['A'-'Z']
let ident = (alpha | '_') (alpha | chiffre | '_')*
let space = [' ' '\t']
let doctal = ['0'-'7']
let dhex= chiffre | ['a'-'f'] | ['A'-'F']
(*TODO : Add caractere/chaine/entier (p2 conventions lexicales)*)


rule token = parse 
    | 'O' (doctal+ as n) {INTEGER ( int_of_octhexstring 8 n)} 
    | '\n' { newline lexbuf;token lexbuf}
    | ident as id { id_or_kwd id}
    | chiffre* as n { INTEGER (int_of_string n) }
(* TODO : Add fonctions OCT/HEX2DEC to correct next rules:*)
(*---------------------------------------------*)
    | "0x" (dhex+ as n) {INTEGER ( int_of_octhexstring 12 n)}
    | "\\x" (dhex dhex as s) {CHARACTER (char_of_int (int_of_string s))}
(*---------------------------------------------*)
    | '"'  {tokstring lexbuf}   (* DONE , VERIF? : add support for string constants *)
    | ''' _ as c ''' {CHARACTER c.[1] }
    | space+ {token lexbuf}
    | "->"  {ARROW}
    | '+' 	{PLUS}          (* on pourrait factoriser*)
    | '*' 	{STAR}         (* cependant on obtiendrait*)
    | '-' 	{MINUS}         (* pas un automate avec moins*)
  	| '/' 	{DIV}           (* d'états. en effet il faudrait*)
  	| '%'   {MOD}           (* matcher une expression reguliere*)
  	| "<="	{LEQ}           (* plus compliquée. ce n'est donc *)
  	| ">="	{GEQ}           (* pas une factorisation aussi utile que*)
  	| "<"   {LT}            (* celle de ident*)
  	| ">"	{GT}
    | "=="  {EQUAL}
  	| "!=" 	{DIFF}
    | '=' 	{GETS}
  	| "||"	{OR}  
  	| "&&"	{AND}
  	| '!'	{NOT}
  	| "++"	{INCR}
  	| "--"	{DECR}
  	| "&"	{AMP}
    | '('	{LPAREN}
    | ')'	{RPAREN}
  	| '['	{LBRA}
  	| ']'	{RBRA}
    | '{'   {LCUR}
    | '}'   {RCUR}
    | '.'   {DOT}
    | "/*"  {comment lexbuf}
    | "//"  {commentendline lexbuf}
    | ','   {COMMA}
    | ';'   {SC}
    | eof   {EOF}
    | _     {raise (Lexing_error "syntax error")}

and tokstring = parse
  |[^   '\\' '"' ''' '\n']* as s {localstring:= (!localstring) ^ s; tokstring lexbuf}
  |'"'                 {let aux = !localstring in localstring:= ""; STRING aux}
  |"\\\""                {localstring:= (!localstring) ^ "\""; 
                       tokstring lexbuf}
  |"\\\'"{localstring:= (!localstring) ^ "\'";
                        tokstring lexbuf} 
  |"\\n" {localstring:= (!localstring)^ "\\n";tokstring lexbuf}
  |"\\\\"   {localstring:= (!localstring) ^ "\\";
                        tokstring lexbuf} 
  |_ as c  {raise (Lexing_error
            (Printf.sprintf "Character %s forbidden"
            (if c = '\n' then "\\n" else String.make 1 c))
            )}

and commentendline = parse
  | '\n' {newline lexbuf; token lexbuf}
  | _ {commentendline lexbuf}

and comment=parse
  |"*/"  {token lexbuf}
  | '\n' { newline lexbuf;comment lexbuf}
  |eof {raise (Lexing_error (Printf.sprintf "Unterminated comment")) }
  |_ {comment lexbuf}



