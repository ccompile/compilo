open Format
open Register
open Print_rtl
open Ltl

let p_linstr f = function
    | Lmove(r1,r2,l) -> fprintf f "move %a %a -> %a"
      Print_rtl.p_pseudoreg r1 p_pseudoreg r2 p_label l
    | LLi(r,n,l) -> fprintf f "li %a %d -> %a"
      p_pseudoreg r (Int32.to_int n) p_label l
    | LLa(r,s,l) -> fprintf f "str %a %a -> %a"
      p_pseudoreg r p_address s p_label l
    | LLw(r,a,l) -> fprintf f "lw %a %a -> %a"
      p_pseudoreg r p_address a p_label l
    | LSw(r,a,l) -> fprintf f "sw %a %a -> %a"
      p_pseudoreg r p_address a p_label l
    | LLb(r,a,l) -> fprintf f "lb %a %a -> %a"
      p_pseudoreg r p_address a p_label l
    | LSb(r,a,l) -> fprintf f "sb %a %a -> %a"
      p_pseudoreg r p_address a p_label l
    | LArith(ar,r1,r2,op,l) -> fprintf f "%a %a %a %a -> %a"
      Mips.print_arith ar p_pseudoreg r1 p_pseudoreg r2 p_operand op p_label l
    | LSet(cond,r1,r2,op,l) -> fprintf f "%a %a %a %a -> %a"
      Mips.print_condition cond p_pseudoreg r1 p_pseudoreg r2 p_operand op p_label l
    | LNeg(r1,r2,l) -> fprintf f "neg %a %a -> %a"
      p_pseudoreg r1 p_pseudoreg r2 p_label l
    | Lgoto(l) -> fprintf f "goto -> %a"
      p_label l
    | LBeq(r1,r2,l1,l2) -> fprintf f "beq %a %a %a -> %a"
      p_pseudoreg r1 p_pseudoreg r2 p_label l1 p_label l2
    | LBeqz(r,l1,l2) -> fprintf f "beqz %a %a -> %a"
      p_pseudoreg r p_label l1 p_label l2
    | LBnez(r,l1,l2) -> fprintf f "bnez %a %a -> %a"
      p_pseudoreg r p_label l1 p_label l2
    | LJr r -> fprintf f "jr %a" p_pseudoreg r
    | Lcall (name,lbl) -> fprintf f "call(%s) -> %a"
      name p_label lbl
    | Lsyscall l -> fprintf f "syscall -> %a"
      p_label l
    | Lget_stack(r,n,l) -> fprintf f "get_stack_param %a %ld -> %a"
      p_pseudoreg r n p_label l
    | Lset_stack(r,n,l) -> fprintf f "set_stack_param %a %ld -> %a"
      p_pseudoreg r n p_label l

let successeurs = function
    | Lmove(_,_,l)
    | LLi(_,_,l)
    | LLa(_,_,l)
    | LLw(_,_,l)
    | LSw(_,_,l)
    | LLb(_,_,l)
    | LSb(_,_,l)
    | LArith(_,_,_,_,l)
    | LSet(_,_,_,_,l)
    | LNeg (_,_,l)
    | Lgoto l
    | Lsyscall l
    | Lget_stack(_,_,l)
    | Lset_stack(_,_,l)
    | Lcall (_,l) -> [l]
    | LBeqz (_,l1,l2)
    | LBnez (_,l1,l2)
    | LBeq (_,_,l1,l2) -> [l1;l2]
    | LJr _ -> []

let rec generic_dfs printer dejavu g f start =
   try
       if not (dejavu.(start)) then
      begin   
           let instr = Ltl.find_instr g start in
           printer start instr;
           dejavu.(start) <- true;
           List.iter (generic_dfs printer dejavu g f) (successeurs instr)
       end
   with Not_found -> ()

let rec ltl_dfs dejavu g f =
    let printer start instr =
        fprintf f "%a : %a\n" p_label start p_linstr instr
    in
    generic_dfs printer dejavu g f 

let p_ldecl f d =
    let dejavu = Array.make (Rtl.max_label ()) false in 
    fprintf f "%s(...):\n%a\n\n"
        d.name
        (ltl_dfs dejavu d.g) d.entry 

let print_ltl f =
    List.iter (p_ldecl f)


