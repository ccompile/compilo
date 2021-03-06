open Register

type operand =
  |Oimm of int
  |Oreg of register

type arith = Add | Sub | Mul | Div | Rem

type condition = Eq | Ne | Le | Lt | Ge | Gt

type label = string

type instruction =
  | Move of register * register
  | Li of register * int
  | Li32 of register * int32
  | La of register * label
  | Lw of register * address
  | Sw of register * address
  | Lb of register * address
  | Sb of register * address
  | Arith of arith * register * register * operand
  | Neg of register * register
  | Set of condition * register * register * operand
  | B of label
  | Beq of register * register * label
  | Bne of register * register * label
  | Beqz of register * label
  | Bnez of register * label
  | J of string
  | Jal of string
  | Jr of register
  | Jalr of register
  | Syscall
  | Label of string
  | Inline of string
  | Nop

type code

val nop : code
val mips : instruction list -> code
val inline : string -> code
val (++) : code -> code -> code

type word = Wint of int | Waddr of string

type data =
  | Asciiz of string * string
  | Word of string * word list
  | Space of string * int
  | Align of int

type program = {
  text : code;
  data : data list;
}

val print_arith : Format.formatter -> arith -> unit
val print_program : Format.formatter -> program -> unit
val print_condition : bool -> Format.formatter -> condition -> unit
val print_instruction : Format.formatter -> instruction -> unit
val print_data : Format.formatter -> data -> unit
val is_immediate : operand -> bool



