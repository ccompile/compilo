open Format
open Rtl
open Register

(* Affichage *)

let p_label f = fprintf f "L%d"

let p_pseudoreg f x =
    fprintf f "%s" (match x with
    | Pseudo n -> sprintf "%%%d" n 
    | ZERO -> "$0"
    | Notreg -> "%X"
    | V0 -> "V0"
    | V1 -> "V1"
    | V2 -> "V2"
    | V3 -> "V3"
    | V4 -> "V4"
    | _ -> "qqch")

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
    | Str(r,s,l) -> fprintf f "str\t%a\t\"%s\"\t-> %a"
      p_pseudoreg r s p_label l
    | Lw(r,a,l) -> fprintf f "lw\t%a\t%a\t\t-> %a"
      p_pseudoreg r p_address a p_label l
    | Sw(r,a,l) -> fprintf f "sw\t%a\t%a\t\t-> %a"
      p_pseudoreg r p_address a p_label l
    | Address(pr1,offset,pr2,l) -> fprintf f "addr\t%a\t%d(%a)\t\t-> %a"
      p_pseudoreg pr1 offset p_pseudoreg pr2 p_label l
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
            | Str(_,_,l)
            | Lw(_,_,l)
            | Sw(_,_,l)
            | Address(_,_,_,l)
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

let p_decl f = function
    | Fct(retval,name,args,g,entry,exit,locals) ->
        let dejavu = Array.make (max_label ()) false in 
        fprintf f "%a %s(%a):\n%a\n\n"
            p_pseudoreg retval name (p_list ", " p_pseudoreg) args 
            (rtl_dfs dejavu g) entry 
    | Glob pr ->
        fprintf f "Global : %a\n\n" p_pseudoreg pr

let p_decl_list f =
    List.iter (p_decl f)

