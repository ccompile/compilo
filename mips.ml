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
  | Beqz of register * label
  | Bnez of register * label
  | J of string
  | Jal of string
  | Jr of register
  | Jalr of register
  | Syscall
  | Label of string
  | Inline of string

type word = Wint of int | Waddr of string

type data =
  | Asciiz of string * string
  | Word of string * word list
  | Space of string * int
  | Align of int

type code =
  | Clist of instruction list
  | Capp of code * code

let nop = Clist []

let mips l = Clist l

let inline s = Clist [Inline s]

let (++) c1 c2 = Capp (c1, c2)

type program = {
  text : code;
  data : data list;
}

open Format
(* open Lib *)

let print_register fmt = function
  | ZERO -> pp_print_string fmt "$0"
  | A0 -> pp_print_string fmt "$a0"
  | A1 -> pp_print_string fmt "$a1"
  | A2 -> pp_print_string fmt "$a2"
  | V0 -> pp_print_string fmt "$v0"
  | T0 -> pp_print_string fmt "$t0"
  | T1 -> pp_print_string fmt "$t1"
  | T2 -> pp_print_string fmt "$t2"
  | S0 -> pp_print_string fmt "$s0"
  | RA -> pp_print_string fmt "$ra"
  | SP -> pp_print_string fmt "$sp"
  | FP -> pp_print_string fmt "$fp"
  | S7 ->  pp_print_string fmt "$s7"
  | S6 ->  pp_print_string fmt "$s6"
  | S5 ->  pp_print_string fmt "$s5"
  | S4 ->  pp_print_string fmt "$s4"
  | S3 ->  pp_print_string fmt "$s3"
  | S2 ->  pp_print_string fmt "$s2"
  | S1 ->  pp_print_string fmt "$s1"
  | T7 ->  pp_print_string fmt "$t7"
  | T6 ->  pp_print_string fmt "$t6"
  | T5 ->  pp_print_string fmt "$t5"
  | T4 ->  pp_print_string fmt "$t4"
  | T3 ->  pp_print_string fmt "$t3"
  | V1 ->  pp_print_string fmt "$v1"
  | _-> assert(false) (*Il resterait des pseudoreg ?! PAS BIEN*)

let print_arith fmt = function
  | Add -> pp_print_string fmt "add"
  | Sub -> pp_print_string fmt "sub"
  | Mul -> pp_print_string fmt "mul"
  | Div -> pp_print_string fmt "div"
  | Rem -> pp_print_string fmt "rem"

let print_condition fmt = function
  | Eq -> pp_print_string fmt "seq"
  | Ne -> pp_print_string fmt "sne"
  | Lt -> pp_print_string fmt "slt"
  | Le -> pp_print_string fmt "sle"
  | Gt -> pp_print_string fmt "sgt"
  | Ge -> pp_print_string fmt "sge"

let print_address fmt = function
  | Alab s -> pp_print_string fmt s
  | Areg (ofs, r) -> fprintf fmt "%d(%a)" ofs print_register r

let print_operand fmt = function
  | Oimm i -> pp_print_int fmt i
  | Oreg r -> print_register fmt r

let print_instruction fmt = function
  | Move (dst, src) ->
      fprintf fmt "\tmove %a, %a\n" print_register dst print_register src
  | Li (r, i) ->
      fprintf fmt "\tli   %a, %d\n" print_register r i
  | Li32 (r, i) ->
      fprintf fmt "\tli   %a, %ld\n" print_register r i
  | La (r, s) ->
      fprintf fmt "\tla   %a, %s\n" print_register r s
  | Lw (r, a) ->
      fprintf fmt "\tlw   %a, %a\n" print_register r print_address a
  | Sw (r, a) ->
      fprintf fmt "\tsw   %a, %a\n" print_register r print_address a
  | Lb (r, a) ->
      fprintf fmt "\tlb   %a, %a\n" print_register r print_address a
  | Sb (r, a) ->
      fprintf fmt "\tsb   %a, %a\n" print_register r print_address a
  | Arith (a, dst, src, op) ->
      fprintf fmt "\t%a  %a, %a, %a\n"
	print_arith a print_register dst print_register src print_operand op
  | Neg (dst, src) ->
      fprintf fmt "\tneg  %a, %a\n" print_register dst print_register src
  | Set (cond, dst, src, op) ->
      fprintf fmt "\t%a  %a, %a, %a\n"
	print_condition cond print_register dst print_register src
	print_operand op
  | B l ->
      fprintf fmt "\tb    %s\n" l
  | Beq (r1, r2,  l) ->
      fprintf fmt "\tbeq  %a, %a, %s\n" print_register r1 print_register r2 l
  | Beqz (r, l) ->
      fprintf fmt "\tbeqz %a, %s\n" print_register r l
  | Bnez (r, l) ->
      fprintf fmt "\tbnez %a, %s\n" print_register r l
  | J s ->
      fprintf fmt "\tj    %s\n" s
  | Jal s ->
      fprintf fmt "\tjal  %s\n" s
  | Jalr r ->
      fprintf fmt "\tjalr %a\n" print_register r
  | Jr r ->
      fprintf fmt "\tjr   %a\n" print_register r
  | Syscall ->
      fprintf fmt "\tsyscall\n"
  | Label s ->
      fprintf fmt "%s:\n" s
  | Inline s ->
      fprintf fmt "%s" s

let rec print_code fmt = function
  | Clist l -> List.iter (print_instruction fmt) l
  | Capp (c1, c2) -> print_code fmt c1; print_code fmt c2

let print_word fmt = function
  | Wint n -> pp_print_int fmt n
  | Waddr s -> pp_print_string fmt s

let comma = ", "

let rec print_list sep printer f = function
  | [] -> ()
  | h::t -> fprintf f "%a%s%a" printer h sep (print_list sep printer) t 

let print_data fmt = function
  | Asciiz (l, s) ->
      fprintf fmt "%s:\n\t.asciiz %S\n" l s
  | Word (l, n) ->
      fprintf fmt "%s:\n\t.word %a\n" l (print_list comma print_word) n
  | Space (l, n) ->
      fprintf fmt "%s:\n\t.space %d\n" l n
  | Align n ->
      fprintf fmt "\t.align %d\n" n

let print_program fmt p =
  fprintf fmt "\t.text\n";
  print_code fmt p.text;
  fprintf fmt "\t.data\n";
  List.iter (print_data fmt) p.data;
  fprintf fmt "@."


