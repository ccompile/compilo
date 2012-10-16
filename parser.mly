
%{


%}

/* mots clés */
%token CHAR, ELSE, FOR, IF, INT, RETURN, SIZEOF, STRUCT, UNION
%token VOID, WHILE 

%token EOF

/* Opérateurs classés par ordre de précédence croissante */
%token GETS 	/* = */
%token OR	/* || */
%token AND	/* && */
%token EQUAL, 	/* == */ DIFF	/* != */
%token LT, 	/* < */  LEQ,	/* <= */  GT,	/* > */  GEQ /* >= */
%token PLUS,	/* + */	 MINUS	/* - */
%token TIMES, 	/* * */  DIV,	/* / */	  MOD	/* % */
%token NOT, 	/* ! */	 INCR,  /* ++ */  DECR, /* -- */ AMP /* & */
%token LPAREN,  /* ( */ RPAREN,	/* ) */ LBRA, /* [ */ RBRA /* ] */

/* Priorités opératoires et associativité */
%right GETS /* a = b = c; signifie a = (b = c) */
%left OR
%left AND
%left EQUAL DIFF /* pour GCC, "a == b == c" signifie "(a == b) == c" */
%left LT LEQ GT GEQ /* idem */
%left PLUS MINUS
%left TIMES DIV MOD /* pour GCC, "3 % 5 % 2" signifie "(3 % 5) % 2" */
%nonassoc NOT INCR DECR AMP
/* je ne suis pas certain de cette dernière ligne… et je ne sais pas */
/* s'il faut aussi définir mettre une ligne pour les parenthèses…    */
/* ça n'a pas trop de sens                                           */

%start fichier

%%

/* pour écrire " <symbole>* " j'écris " <symbole>s " et j'ajoute la règle
   correspondante */

fichier:
    | l = decls EOF     { l } /* faut-il un List.rev ? */
    ;

decls:
    | h = decl            { [h] }
    | t = decls h = decl  { h::t }
    ;




