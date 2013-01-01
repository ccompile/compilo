
(* Label : noeud dans le graphe de flot de contrôle *)
type label

type pseudoreg

(* Comme dans mips.ml *)
type address =
  | Alab of string
  | Areg of int * pseudoreg

type operand =
  | Oimm of int32
  | Oreg of pseudoreg

(* Format des instructions RTL *)
type instr =
  | Move of pseudoreg * pseudoreg * label
  | Li   of pseudoreg * int32 * label
  | La   of pseudoreg * label * label
  | Lw   of pseudoreg * address * label
  | Sw   of pseudoreg * address * label
  | Arith of Mips.arith * pseudoreg * pseudoreg * operand * label
  | Neg  of pseudoreg * pseudoreg * label
  | B    of label
  | Beq  of pseudoreg * pseudoreg * label * label
  | Beqz of pseudoreg * label * label
  | Bnez of pseudoreg * label * label
  | Return of pseudoreg
  | Call of string * pseudoreg list * pseudoreg * label
  | Putchar of pseudoreg (*argument*) * pseudoreg (*valeur de retour*) * label
  | Sbrk of pseudoreg (*argument*) * pseudoreg (*valeur de retour*) * label

type graph

(* Dans Fct, le dernier label est le point d'entrée de la fonction *)
type decl =
  | Fct of Types.expr_type * string * (pseudoreg list) * graph * label
  | Glob of pseudoreg

type local_env

val compile_fichier : Types.wfichier -> decl list

val print_rtl : Format.formatter -> graph -> unit

val fresh_label : unit -> label

val fresh_pseudoreg : unit -> pseudoreg

val generate : instr -> label

