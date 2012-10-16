{
  open Lexing
  open Parser
  exception Lexing_error of string
  let kwd_tbl =
  	["char",CHAR;"else",ELSE;"for",FOR;"if",IF;"int",INT;"return",RETURN;
	  "sizeof",SIZEOF;"struct",STRUCT;"union",UNION;"void",VOID;"while",WHILE]
  let id_or_kwd s = try List.assoc s kwd_tbl with _-> IDENT s
	(*A voir l'utilité d'une fonction newline, qui permet de
	repositionner le curseur en cas de passage à la ligne dans un
	fichier*)
}

let chiffre = [0-9]
let alpha = [a-z] | [A-Z]
let ident = (alpha | _) (alpha | chiffre | _)*
let space = [' ' '\t']

rule token = parse 
	|'\n' {token lexbuf}
	|ident as if {id_or_kwd id}
	|space+ {token lexbuf}
	|'+'	{PLUS}          /* on pourrait factoriser*/
	|'*'	{TIMES}         /* cependant on obtiendrait*/
	|'-' 	{MINUS}         /* pas un automate avec moins*/
	|'/'	{DIV}           /* d'états. en effet il faudrait*/
	|'%'	{MOD}           /* matcher une expression reguliere*/
	|"<="	{LEQ}           /* plus compliquée. ce n'est donc */
	|">="	{GEQ}           /* pas une factorisation aussi utile que*/
	|"<"	{LT}            /* celle de ident*/
	|">"	{GT}
	|"=="	{EQUAL}
	|"!=" 	{DIFF}
	|"=" 	{GETS}
	|"||"	{OR}
	|"&&"	{AND}
	|'!'	{NOT}
	|"++"	{INCR}
	|"--"	{DECR}
	|"&"	{AMP}
	|'('	{LPAREN}
	|')'	{RPAREN}
	|'['	{LBRA}
	|']'	{RBRA}

