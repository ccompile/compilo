open Format
open Rtl
open Register

(* Affichage *)

let p_label f = fprintf f "L%d"

let p_pseudoreg f x =
    fprintf f "%s" (match x with
    | Pseudo n -> sprintf "%%%d" n 
    | ZERO -> "$0"
    | V0 -> "V0"
    | V1 -> "V1"
    | T0 -> "T0"
    | T1 -> "T1"
    | T2 -> "T2"
    | T3 -> "T3"
    | T4 -> "T4"
    | T5 -> "T5"
    | T6 -> "T6"
    | T7 -> "T7"
    | SP -> "SP"
    | FP -> "FP"
    | S0 -> "S0"
    | S1 -> "S1"
    | S2 -> "S2"
    | S3 -> "S3"
    | S4 -> "S4"
    | S5 -> "S5"
    | S6 -> "S6"
    | S7 -> "S7"
    | A0 -> "A0"
    | A1 -> "A1"
    | A2 -> "A2"
    | RA -> "RA")

let p_address f = function
    | Alab s -> fprintf f "%s" s
    | Areg (n,pr) -> fprintf f "%d(%a)" n p_pseudoreg pr

let p_operand f = function
    | Oimm n -> fprintf f "%d" (Int32.to_int n)
    | Oreg pr -> p_pseudoreg f pr

let rec p_list sep printer f = function
    | [] -> ()
    | [h] -> printer f h
    | h::t -> fprintf f "%a%s%a"
              printer h
              sep
              (p_list sep printer) t

let p_instr f = function
    | Move(r1,r2,l) -> fprintf f "move\t%a\t%a\t-> %a"
      p_pseudoreg r1 p_pseudoreg r2 p_label l
    | Li(r,n,l) -> fprintf f "li \t%a\t%d\t-> %a"
      p_pseudoreg r (Int32.to_int n) p_label l
    | La(r,s,l) -> fprintf f "str\t%a\t%a\t-> %a"
      p_pseudoreg r p_address s p_label l
    | Lw(r,a,l) -> fprintf f "lw\t%a\t%a\t\t-> %a"
      p_pseudoreg r p_address a p_label l
    | Sw(r,a,l) -> fprintf f "sw\t%a\t%a\t\t-> %a"
      p_pseudoreg r p_address a p_label l
    | Lb(r,a,l) -> fprintf f "lb\t%a\t%a\t\t-> %a"
      p_pseudoreg r p_address a p_label l
    | Sb(r,a,l) -> fprintf f "sb\t%a\t%a\t\t-> %a"
      p_pseudoreg r p_address a p_label l
    | Address(pr1,pr2,l) -> fprintf f "addr\t%a\t[%a]\t\t-> %a"
      p_pseudoreg pr1 p_pseudoreg pr2 p_label l
    | Arith(ar,r1,r2,op,l) -> fprintf f "%a %a\t%a\t%a\t-> %a"
      Mips.print_arith ar p_pseudoreg r1 p_pseudoreg r2 p_operand op p_label l
    | Set(cond,r1,r2,op,l) -> fprintf f "%a %a\t%a\t%a\t-> %a"
      Mips.print_condition cond p_pseudoreg r1 p_pseudoreg r2 p_operand op p_label l
    | Neg(r1,r2,l) -> fprintf f "neg\t%a\t%a\t\t-> %a"
      p_pseudoreg r1 p_pseudoreg r2 p_label l
    | B(l) -> fprintf f "b\t\t\t-> %a"
      p_label l
    | Beq(r1,r2,l1,l2) -> fprintf f "beq\t%a\t%a\t%a\t-> %a"
      p_pseudoreg r1 p_pseudoreg r2 p_label l1 p_label l2
    | Beqz(r,l1,l2) -> fprintf f "beqz\t%a\t%a\t\t-> %a"
      p_pseudoreg r p_label l1 p_label l2
    | Bnez(r,l1,l2) -> fprintf f "bnez\t%a\t%a\t-> %a"
      p_pseudoreg r p_label l1 p_label l2
    | Return(Some r,lbl) -> fprintf f "return\t%a\t\t-> %a"
      p_pseudoreg r p_label lbl
    | Return(None,lbl) -> fprintf f "return\t\t\t-> %a"
      p_label lbl
    | Call (name,args,destreg,lbl) -> fprintf f "%a := %s(%a)\t\t-> %a"
        p_pseudoreg destreg name (p_list "," p_pseudoreg) args p_label lbl
    | Putchar(arg,retval,lbl) -> fprintf f "%a := putchar(%a)\t\t-> %a"
        p_pseudoreg retval p_pseudoreg arg p_label lbl
    | Sbrk(arg,retval,lbl) -> fprintf f "%a := sbrk(%a)\t\t-> %a"
        p_pseudoreg retval p_pseudoreg arg p_label lbl

let print_rtl f g =
    let p_binding l i =
       fprintf f "%a : %a\n" p_label l p_instr i in
    iter_instr g p_binding

let rec rtl_dfs dejavu g f start =
   try
       if not (dejavu.(start)) then
       begin
           let instr = find_instr g start in
           fprintf f "%a : %a\n" p_label start p_instr instr;
           dejavu.(start) <- true;
           (match instr with
            | Move(_,_,l)
            | Li(_,_,l)
            | La(_,_,l)
            | Lw(_,_,l)
            | Sw(_,_,l)
            | Lb(_,_,l)
            | Sb(_,_,l)
            | Address(_,_,l)
            | Arith(_,_,_,_,l)
            | Set(_,_,_,_,l)
            | Neg (_,_,l)
            | B l
            | Call (_,_,_,l)
            | Putchar (_,_,l)
            | Sbrk (_,_,l) -> rtl_dfs dejavu g f l
            | Beqz (_,l1,l2)
            | Bnez (_,l1,l2)
            | Beq (_,_,l1,l2) -> rtl_dfs dejavu g f l1; rtl_dfs dejavu g f l2
            | Return _ -> ())
       end
   with Not_found -> ()

let p_decl f d =
    let dejavu = Array.make (max_label ()) false in 
       fprintf f "%a %s(%a):\nentry : %a\nexit : %a\n%a\n\n"
            p_pseudoreg d.retval d.name (p_list ", " p_pseudoreg) d.args 
            p_label d.entry p_label d.exit
            (rtl_dfs dejavu d.g) d.entry 

let p_decl_list f =
    List.iter (p_decl f)

