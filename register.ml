type register =
  | V0
  | V1
  | ZERO
  | T0
  | T1
  | T2
  | T3
  | T4
  | T5
  | T6
  | T7
  | SP
  | FP
  | S0
  | S1
  | S2
  | S3
  | S4
  | S5
  | S6
  | S7
  | A0
  | A1
  | A2
  | Pseudo of int
  | RA 

type address =
  | Alab of string
  | Areg of int32 * register


(* Compilator options*)

let parameters = [A0;A1;A2]   
let result = V0 
let ra = RA  
let callee_saved = [S0;S1;S2;S3;S4;S5;S6;S7]
let caller_saved = [T0;T1;T2;T3;T4;T5;T6] 

(* pour syscall : *)
let a0 = A0
let v0 = V0

module Rset = Set.Make(struct type t = register
    let compare = compare end)

type set = Rset.t

let available_registers =
    [ V0; T0; T1; T2; T3; T4; T5; T6;
      S0; S1; S2; S3; S4; S5; S6; S7; A0;
      A1; A2; RA ] 

let is_physical = function
   | Pseudo _ -> false
   | _ -> true
let sp=SP

