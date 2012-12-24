
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
  | Beq  of pseudoreg * pseudoreg * label
  | Beqz of pseudoreg * label
  | Bnez of pseudoreg * label
  | Jr   of pseudoreg
  | Syscall of label

type graph

val compile_expr : graph -> pseudoreg -> label -> Types.texpr -> unit

val compile_instr : graph -> label -> Types.winstr -> unit


