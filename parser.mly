
%{

    open Ast
%}

/* mots clés */
%token CHAR, ELSE, FOR, IF, INT, RETURN, SIZEOF, STRUCT, UNION
%token VOID, WHILE 
%token <string> IDENT
%token <string> STRING
%token <int>    INTEGER
%token <char>   CHARACTER
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

%token COMMA    /* , */
%token LPAREN,  /* ( */ RPAREN,	/* ) */ LBRA, /* [ */ RBRA /* ] */
%token LCUR,    /* { */ RCUR    /* } */
%token SC,      /* ; */ DOT     /* . */

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

%start<Ast.afichier> fichier

%%

fichier:
    | l = decl* EOF     { l } 
    ;

decl:
    | t=decl_vars           { Adecl_vars t }
    | t=decl_typ            { Adecl_typ t }
    | t=decl_fct            { Adecl_fct t }
    ;

decl_fct:
   | t=typ (* s=TIMES* *) i=IDENT LPAREN args=separated_list(COMMA,argument)
   RPAREN b=bloc   { (t, (* (List.length s) *)0, i, (* args*) [], b) }
   ;

decl_vars:
   | t=typ v=var* SC        { t,v }
   ;

decl_typ:
   | STRUCT s=IDENT LCUR d=decl_vars* RCUR SC { false, s, d }
   | UNION  s=IDENT LCUR d=decl_vars* RCUR SC { true, s, d }
   ;

typ:
  | VOID                    { A_void }
  | INT                     { A_int }
  | CHAR                    { A_char }
  | STRUCT s=IDENT          { A_struct s }
  | UNION s=IDENT           { A_union s }
  ;

argument:
  | t=typ v=var             { t,v }
  ;

var:
  | s=IDENT                 { AV_ident s }
  | TIMES v=var             { AV_star v } (* TODO : replace TIMES with STAR *)
  ;

expr:
   | e=INTEGER                  { AE_int e }
   | i=IDENT                    { AE_ident i }
   | c=STRING                   { AE_str c }
   | c=CHARACTER                { AE_int (int_of_char c) }
   (* TODO : add an AE_char node ? *)
   | TIMES e=expr               { AE_star e }
   | e1=expr LBRA e2=expr RBRA  { AE_brackets (e1,e2) }
   | e1=expr DOT e2=expr        { AE_dot(e1,e2) }
   | e=expr MINUS GT s=IDENT    { AE_arrow(e,s) }
    (* TODO : create ARROW terminal symbol ? *)
   | e1=expr GETS e2=expr       { AE_gets(e1,e2) }
   | s=IDENT LPAREN args=separated_list(COMMA,expr) RPAREN
                                { AE_call(s,args) }
   | INCR e=expr                { AE_incr(IncrRet,e) }
   | DECR e=expr                { AE_incr(DecrRet,e) }
   | e=expr INCR                { AE_incr(RetIncr,e) }
   | e=expr DECR                { AE_incr(RetDecr,e) }
   | AMP e=expr                 { AE_unop(AU_addr,e) }
   | NOT e=expr                 { AE_unop(AU_not,e) }
   | MINUS e=expr               { AE_unop(AU_minus,e) }
   | PLUS e=expr                { AE_unop(AU_plus,e) }
   | e1=expr o=operateur e2=expr{ AE_binop(o,e1,e2) }
   | SIZEOF LPAREN t=typ s=TIMES* RPAREN { AE_sizeof(t, List.length s) }
   | LPAREN e=expr RPAREN       { e }
   ;

operateur:
   | EQUAL      { AB_equal }
   | DIFF       { AB_diff }
   | LT         { AB_lt }
   | LEQ        { AB_leq }
   | GT         { AB_gt }
   | GEQ        { AB_geq }
   | PLUS       { AB_plus }
   | MINUS      { AB_minus }
   | TIMES      { AB_times }
   | DIV        { AB_div }
   | MOD        { AB_mod }
   | AND        { AB_and }
   | OR         { AB_or }
   ;

instruction:
   | SC      { AI_none }
   | e=expr SC  { AI_inst(e) }
   | IF LPAREN e=expr RPAREN i=instruction
                { AI_if(e,i) }
   | IF LPAREN e=expr RPAREN i1=instruction ELSE i2=instruction
                { AI_if_else(e,i1,i2) }
   | WHILE LPAREN e=expr RPAREN i=instruction
                { AI_while(e,i) }
   | FOR LPAREN init=separated_list(COMMA,expr) SC check=option(expr) SC
     incr=separated_list(COMMA,expr) RPAREN i=instruction
                { AI_for(init,check,incr,i) }
   | b=bloc     { AI_bloc(b) }
   | RETURN retval=option(expr) SC { AI_return(retval) }
   ;

bloc:
   | LCUR v=decl_vars* i=instruction* RCUR
                { v,i }
   ;






