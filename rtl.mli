
type operand =
  | Oimm of int
  | Oreg of pseudoreg

type pseudoreg = int

type instruction =
  | Move of pseudoreg * pseudoreg
  | Add of pseudoreg * pseudoreg * pseudoreg * operand 

(* TBC *)


