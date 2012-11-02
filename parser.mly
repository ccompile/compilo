
%{
    open Ast
    let pr = Printf.printf "%s\n"
    
    let string_of_label lb =
        let cend = (if lb.cbegin < lb.cend then lb.cend else lb.cbegin+1) in
            Format.sprintf "File %s, line %d, characters %d-%d:\n"
            lb.file
            lb.line
            lb.cbegin
            cend

    let rec int_lident_of_var = function
        | AV_ident i -> (0,i)
        | AV_star s ->let (n,i) = int_lident_of_var s in (n+1,i)
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
%token STAR, 	/* * */  DIV,	/* / */	  MOD	/* % */
%token NOT, 	/* ! */	 INCR,  /* ++ */  DECR, /* -- */ AMP /* & */

%token COMMA    /* , */
%token LPAREN,  /* ( */ RPAREN,	/* ) */ LBRA, /* [ */ RBRA /* ] */
%token LCUR,    /* { */ RCUR    /* } */
%token SC,      /* ; */ DOT     /* . */

/* Priorités opératoires et associativité */
%right GETS /* a = b = c; signifie a = (b = c) */
%left AND 
%left OR
%left  EQUAL DIFF /* pour GCC, "a == b == c" signifie "(a == b) == c" */
%left LT LEQ GT GEQ /* idem */
%left STAR
%left PLUS MINUS DIV MOD
%left INCR DECR
%left DOT  
%nonassoc NOT /*ceux sont les symboles non associatifs que l'on réduit directement*/
%nonassoc LBRA
%nonassoc AMP

%start<Ast.afichier> fichier

%%

labeled(X):
    | x = X { let sp = $startpos.Lexing.pos_bol in
             {file=($startpos.Lexing.pos_fname);
              line=($startpos.Lexing.pos_lnum);
              cbegin=($startpos.Lexing.pos_cnum - sp);
              cend=($endpos.Lexing.pos_cnum - sp)}, x }

fichier:
    | l = decl* EOF     { l } 
    ;

decl:
    | t=labeled(decl_fct)            { fst t, Adecl_fct (snd t) }
    | t=labeled(decl_vars)           { fst t, Adecl_vars (snd t) } 
    | t=labeled(decl_typ)            { fst t, Adecl_typ (snd t) }
    ;

decl_fct:
   | t=labeled(typ) v=var
     LPAREN args=separated_list(COMMA,labeled(argument))
     RPAREN b=labeled(bloc)
     {let (n,i) = int_lident_of_var v in
         (t, n, i, args, b) }
   ;

decl_vars:
   | t=labeled(typ) v=separated_list(COMMA,var) SC        { t,v }
   ;

decl_typ:
   | STRUCT s=labeled(IDENT) LCUR d=labeled(decl_vars)* RCUR SC { false, s, d }
   | UNION  s=labeled(IDENT) LCUR d=labeled(decl_vars)* RCUR SC { true, s, d }
   ;

typ:
  | VOID                    { A_void }
  | INT                     { A_int }
  | CHAR                    { A_char }
  | STRUCT s=labeled(IDENT) { A_struct s }
  | UNION s=labeled(IDENT)  { A_union s }
  ;

argument:
  | t=labeled(typ) v=var             { t,v }
  ;

var:
  | s=labeled(IDENT)       { AV_ident s }
  | STAR v=var             { AV_star v } 
  ;

expr:
   | e=INTEGER                  { AE_int e }
   | i=IDENT                    { AE_ident i }
   | c=STRING                   { AE_str c }
   | c=CHARACTER                { AE_int (int_of_char c) }
   (* TODO : add an AE_char node ? *)
   | STAR e=labeled(expr)       { AE_star e }
   | e1=labeled(expr) LBRA
     e2=labeled(expr) RBRA      { AE_brackets (e1,e2) }
   | e1=labeled(expr) DOT
     e2=labeled(expr)           { AE_dot(e1,e2) }
   | e=labeled(expr) MINUS
     GT s=labeled(IDENT)        { AE_arrow(e,s) } 
     (* TODO : create ARROW terminal symbol ? *)
   | e1=labeled(expr) GETS
     e2=labeled(expr)           { AE_gets(e1,e2) }
   | s=labeled(IDENT) LPAREN
     args=separated_list(COMMA,labeled(expr)) RPAREN
                                { AE_call(s,args) }
   | INCR e=labeled(expr)       { AE_incr(IncrRet,e) }
   | DECR e=labeled(expr)       { AE_incr(DecrRet,e) }
   | e=labeled(expr) INCR       { AE_incr(RetIncr,e) }
   | e=labeled(expr) DECR       { AE_incr(RetDecr,e) }
   | AMP e=labeled(expr)        { AE_unop(AU_addr,e) }
   | NOT e=labeled(expr)        { AE_unop(AU_not,e) }
   | MINUS e=labeled(expr)      { AE_unop(AU_minus,e) }
   | PLUS e=labeled(expr)       { AE_unop(AU_plus,e) }
   | e1=labeled(expr) o=operateur
     e2=labeled(expr)           { AE_binop(o,e1,e2) }
   | SIZEOF LPAREN t=labeled(typ)
     s=STAR* RPAREN             { AE_sizeof(t, List.length s) }
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
   | STAR       { AB_times }
   | DIV        { AB_div }
   | MOD        { AB_mod }
   | AND        { AB_and }
   | OR         { AB_or }
   ;

instruction:
   | SC         {AI_none }
   | e=expr SC  {AI_inst(e) }
   | IF LPAREN e=labeled(expr) RPAREN i=labeled(instruction)
                {AI_if(e,i) }
   | IF LPAREN e=labeled(expr) RPAREN i1=labeled(instruction) ELSE i2=labeled(instruction)
                { AI_if_else(e,i1,i2) }
   | WHILE LPAREN e=labeled(expr) RPAREN i=labeled(instruction)
                { AI_while(e,i) }
   | FOR LPAREN init=separated_list(COMMA,labeled(expr)) SC
    check=option(labeled(expr)) SC
    incr=separated_list(COMMA,labeled(expr)) RPAREN i=labeled(instruction)
                { AI_for(init,check,incr,i) }
   | b=bloc     { AI_bloc(b) }
   | RETURN retval=option(labeled(expr)) SC { AI_return(retval) }
   ;

bloc:
   | LCUR v=labeled(decl_vars)* i=labeled(instruction)* RCUR
                { v,i }
   ;

