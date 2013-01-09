{
  open Lexing
  open Ast
  open Parser
  open Errors
  open Int32
  exception Lexing_error of string 
  exception Eof
  let kwd_tbl =
  	["char",CHAR;"else",ELSE;"for",FOR;
	"if",IF;"int",INT;"return",RETURN;
	"sizeof",SIZEOF;"struct",STRUCT;
	"union",UNION;"void",VOID;"while",WHILE]
  let id_or_kwd s = try List.assoc s kwd_tbl with _-> IDENT s

  let localstring=ref ""
	
  let newline lexbuf =
    let pos= lexbuf.lex_curr_p in
    lexbuf.lex_curr_p<-
    	{ pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }  

  let pr = Printf.printf "%s\n"
(*

(*Fonctions utilisées précedement avant la découverte du miracle
of_string "0xA" donne 10*)

let asci2int = function
  |'a'|'A'->  10
  |'b'|'B'->  11
  |'c'|'C'->  12
  |'d'|'D'->  13
  |'e'|'E'->  14
  |'f'|'F'->  15
  |_ as c->  c- 48 
 
let int_of_octhexstring base st = 
  let s= of_int (String.length st) in
  let puis = ref 1 in
  let res = ref 0 in
  for i= s-1 downto  0 do
    res := !res + (asci2int st.[i]) * !puis;
    puis := base * !puis;
  done;
  !res
*)
}


let chiffre = ['0'-'9']
let pchif = ('1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9')
let nombre = (pchif) (chiffre)* 
let alpha = ['a'-'z'] | ['A'-'Z']
let ident = (alpha | '_') (alpha | chiffre | '_')*
let space = [' ' '\t']
let doctal = ['0'-'7']
let dhex= chiffre | ['a'-'f'] | ['A'-'F']

rule token = parse 
    | '0' (doctal+ as n) {INTEGER ( of_string ("0o"^n))} 
    | '\n' { newline lexbuf;token lexbuf}
    | ident as id { id_or_kwd id}
    | (nombre|'0') as n { INTEGER (of_string n) }
    | "0x" (dhex+ as n) {INTEGER (of_string  ("0x"^n))}
    | '"'  {tokstring lexbuf}   
    | ''' { CHARACTER (tokchar lexbuf) }
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
  |[^   '\\' '"' ''' '\n']* as s 
  	{localstring:= (!localstring) ^ s; tokstring lexbuf}
  |'"'                 
  	{let aux = !localstring in localstring:= ""; STRING aux}
  |"\\\""                {localstring:= (!localstring) ^ "\""; 
                       tokstring lexbuf}
  |"\\\'"{localstring:= (!localstring) ^ "\'";
                        tokstring lexbuf} 
  |"\\n" {localstring:= (!localstring)^ "\\n";tokstring lexbuf}
  |"\\\\"   {localstring:= (!localstring) ^ "\\";
                        tokstring lexbuf}
  | "\\x" (dhex dhex as s) 
  	{localstring := (!localstring) ^
			(String.make 1 (char_of_int
  			(int_of_string ("0x"^s)))); tokstring lexbuf}
  |eof {raise(Lexing_error("Unterminated string"))}
  |_ as c  {raise (Lexing_error
            (Printf.sprintf "Character %s forbidden"
            (if c = '\n' then "newline" else String.make 1 c))
            )}

and tokchar = parse
  | "\\x" (dhex dhex as s) "'" { (char_of_int (int_of_string ("0x"^s))) }
  | [^ '\\'] as c "'"                 { c }
  | eof {raise(Lexing_error("Unterminated char"))}
  | _                          
  	{ raise (Lexing_error ("Invalid character")) }

and commentendline = parse
  | '\n' {newline lexbuf; token lexbuf}
  |eof {token lexbuf}
  | _ {commentendline lexbuf}

and comment=parse
  |"*/"  {token lexbuf}
  | '\n' { newline lexbuf;comment lexbuf}
  |eof {raise (Lexing_error ("Unterminated comment")) }
  |_ {comment lexbuf}



