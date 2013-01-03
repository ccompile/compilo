
(* Label : noeud dans le graphe de flot de contrôle *)
type label = int

val notlabel : label

type pseudoreg = Register.register

(* Comme dans mips.ml *)
type address =
  | Alab of string
  | Areg of int * pseudoreg

type operand =
  | Oimm of int32
  | Oreg of pseudoreg

(* Format des instructions RTL *)
type instr =
  (* Move(r1,r2,l) : r2 <- r1 *)
  | Move of pseudoreg * pseudoreg * label
  | Li   of pseudoreg * int32 * label
  | Str  of pseudoreg * string * label
  (* Lw et Sw ne sont pas ceux de MIPS ! ils représentent aussi Lb et Sb *)
  (* et il faut gérer les structures *)
  | Lw   of pseudoreg * address * label
  | Sw   of pseudoreg * address * label
  (* Address(r1,o,r2,l) : r1 <- &r2 + o
   * On garantit que r2 est une variable (locale ou globale) *)
  | Address of pseudoreg * int (*offset*) * pseudoreg * label
  | Arith of Mips.arith * pseudoreg * pseudoreg * operand * label
  | Set of Mips.condition * pseudoreg * pseudoreg * operand * label
  | Neg  of pseudoreg * pseudoreg * label
  | B    of label
  | Beq  of pseudoreg * pseudoreg * label * label
  | Beqz of pseudoreg * label * label
  | Bnez of pseudoreg * label * label
  | Return of pseudoreg option * label
  | Call of string * pseudoreg list * pseudoreg * label
  | Putchar of pseudoreg (*argument*) * pseudoreg (*valeur de retour*) * label
  | Sbrk of pseudoreg (*argument*) * pseudoreg (*valeur de retour*) * label

type graph

val find_instr : graph -> label -> instr

val iter_instr : graph -> (label -> instr -> unit) -> unit

val max_label : unit -> label

(* Dans Fct, le dernier label est le point d'entrée de la fonction *)
type decl =
  | Fct of pseudoreg (* retval *) * string (* name *) * (pseudoreg list) * graph
  * label (* entry *) * label (* exit *) * Register.set (* locals *)
  | Glob of pseudoreg

val compile_fichier : Types.wfichier -> decl list

val fresh_label : unit -> label

val fresh_pseudoreg : unit -> pseudoreg

val generate : instr -> label

