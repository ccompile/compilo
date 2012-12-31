
(* TODO : commenter *)

type label

type pseudoreg

type address =
  | Alab of string
  | Areg of int * pseudoreg

type operand =
  | Oimm of int
  | Oreg of pseudoreg

type instr =
  | Move of pseudoreg * pseudoreg * label
  | Li   of pseudoreg * int32 * label
  | La   of pseudoreg * label * label
  | Lw   of pseudoreg * address * label
  | Sw   of pseudoreg * address * label
  | Arith of Mips.arith * pseudoreg * pseudoreg * operand * label
  | Neg  of pseudoreg * pseudoreg * label
(*| Set *)
  | B    of label
  | Beq  of pseudoreg * pseudoreg * label * label
  | Beqz of pseudoreg * label * label
  | Bnez of pseudoreg * label * label
  | Jr   of pseudoreg * label
  | Call of string * pseudoreg list * pseudoreg * label

type graph

type local_env

val compile_fichier : Types.wfichier -> graph

val print_rtl : Format.formatter -> graph -> unit

