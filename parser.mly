
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
%token NOT, 	/* ! */	 INCR,  /* ++ */  DECR, /* -- */ AMP, /* & */
%token 

