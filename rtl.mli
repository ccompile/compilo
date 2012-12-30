
(* TODO : commenter *)

type label

type pseudoreg =
    | Notreg (* registre fantoche, qui stocke des void, par exemple *)
    | V0
    | A0
    | Pseudo of int

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
  | Syscall of label

type graph

type local_env = pseudoreg Types.Env.t

val compile_expr : graph ref -> local_env -> pseudoreg -> label -> label -> Types.texpr -> unit

val compile_instr : graph ref -> local_env ref -> label -> label -> Types.winstr -> unit

val compile_fichier : Types.wfichier -> graph

val print_rtl : Format.formatter -> graph -> unit

