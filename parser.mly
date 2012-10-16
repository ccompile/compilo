
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
/* Les deux lignes suivantes ont la même précédence */
%token NOT, 	/* ! */	 INCR,  /* ++ */  DECR, /* -- */ AMP, /* & */
%token UTIMES,  /* * (unaire) */ UPLUS, /* + (unaire) */ UMINUS
	/* - (unaire) */
%token LPAREN,  /* ( */ RPAREN,	/* ) */ LBRA, /* [ */ RBRA /* ] */


