{
  open Lexing
  open Ast
  open Parser
  exception Lexing_error of string 
  exception Eof
  let kwd_tbl =
  	["char",CHAR;"else",ELSE;"for",FOR;"if",IF;"int",INT;"return",RETURN;
	  "sizeof",SIZEOF;"struct",STRUCT;"union",UNION;"void",VOID;"while",WHILE]
  let id_or_kwd s = try List.assoc s kwd_tbl with _-> IDENT s

  let newline lexbuf =
    let pos= lexbuf.lex_curr_p in
    lexbuf.lex_curr_p<-{ pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }  
}


let chiffre = ['0'-'9']
let alpha = ['a'-'z'] | ['A'-'Z']
let ident = (alpha | '_') (alpha | chiffre | '_')*
let space = [' ' '\t']

(* TODO : add support for string constants *)

rule token = parse 
    | '\n' { newline lexbuf;token lexbuf}
    | ident as id {id_or_kwd id}
    | chiffre* as n { INTEGER (int_of_string n) }
    | ''' _ as c ''' { CHARACTER c.[0] }
    | space+ {token lexbuf}
    | '+' 	{PLUS}          (* on pourrait factoriser*)
    | '*' 	{TIMES}         (* cependant on obtiendrait*)
    | '-' 	{MINUS}         (* pas un automate avec moins*)
	| '/' 	{DIV}           (* d'états. en effet il faudrait*)
	| '%'	  {MOD}           (* matcher une expression reguliere*)
	| "<="	{LEQ}           (* plus compliquée. ce n'est donc *)
	| ">="	{GEQ}           (* pas une factorisation aussi utile que*)
	| "<"	  {LT}            (* celle de ident*)
	| ">"	  {GT}
	| "=="	{EQUAL}
	| "!=" 	{DIFF}
    | '=' 	{GETS}
	| "||"	{OR}
	| "&&"	{AND}
	| '!'	  {NOT}
	| "++"	{INCR}
	| "--"	{DECR}
	| "&"	  {AMP}
    | '('	  {LPAREN}
    | ')'	  {RPAREN}
	| '['	  {LBRA}
	| ']'	  {RBRA}
    | '{'   {LCUR}
    | '}'   {RCUR}
    | '.'   {DOT}
    | ','   {COMMA}
    | ';'   {SC}
    | eof   {EOF}
    | _     {raise (Lexing_error
            (Printf.sprintf "File %s, line %i, characters %i: \n Syntax error"
            (Sys.argv.(2))
            (lexbuf.lex_curr_p.pos_lnum)
            (lexbuf.lex_curr_p.pos_bol))
            )}

