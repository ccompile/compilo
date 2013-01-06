open Format
open Register
open Print_rtl
open Ltl

let p_linstr f = function
    | Lmove(r1,r2,l) -> fprintf f "move %a %a -> %a"
      Print_rtl.p_pseudoreg r1 p_pseudoreg r2 p_label l
    | LLi(r,n,l) -> fprintf f "li %a %d -> %a"
      p_pseudoreg r (Int32.to_int n) p_label l
    | LStr(r,s,l) -> fprintf f "str %a \"%s\" -> %a"
      p_pseudoreg r s p_label l
    | LLw(r,a,l) -> fprintf f "lw %a %a -> %a"
      p_pseudoreg r p_address a p_label l
    | LSw(r,a,l) -> fprintf f "sw %a %a -> %a"
      p_pseudoreg r p_address a p_label l
    | LLb(r,a,l) -> fprintf f "lb %a %a -> %a"
      p_pseudoreg r p_address a p_label l
    | LSb(r,a,l) -> fprintf f "sb %a %a -> %a"
      p_pseudoreg r p_address a p_label l
    | LAddress(pr1,offset,pr2,l) -> fprintf f "addr %a %d(%a) -> %a"
      p_pseudoreg pr1 offset p_pseudoreg pr2 p_label l
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
    | LReturn -> fprintf f "return"
    | LJr r -> fprintf f "jr %a" p_pseudoreg r
    | Lcall (name,n,lbl) -> fprintf f "call(%s,%d) -> %a"
      name n p_label lbl
    | Lsyscall l -> fprintf f "syscall -> %a"
      p_label l
    | Lalloc_frame l -> fprintf f "alloc_frame -> %a"
      p_label l
    | Ldelete_frame l -> fprintf f "delete_frame -> %a"
      p_label l
    | Lget_stack(r,n,l) -> fprintf f "get_stack_param %a %d -> %a"
      p_pseudoreg r n p_label l
    | Lset_stack(r,n,l) -> fprintf f "set_stack_param %a %d -> %a"
      p_pseudoreg r n p_label l

let successeurs = function
    | Lmove(_,_,l)
    | LLi(_,_,l)
    | LStr(_,_,l)
    | LLw(_,_,l)
    | LSw(_,_,l)
    | LLb(_,_,l)
    | LSb(_,_,l)
    | LAddress(_,_,_,l)
    | LArith(_,_,_,_,l)
    | LSet(_,_,_,_,l)
    | LNeg (_,_,l)
    | Lgoto l
    | Lsyscall l
    | Lget_stack(_,_,l)
    | Lset_stack(_,_,l)
    | Lcall (_,_,l) -> [l]
    | LBeqz (_,l1,l2)
    | LBnez (_,l1,l2)
    | LBeq (_,_,l1,l2) -> [l1;l2]
    | LReturn
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

let p_ldecl f = function
    | Ltl.Fct(name,entry,g) ->
        let dejavu = Array.make (Rtl.max_label ()) false in 
        fprintf f "%s(...):\n%a\n\n"
            name
            (ltl_dfs dejavu g) entry 
    | Ltl.Glob pr ->
         fprintf f "Global : %a\n\n" p_pseudoreg pr

let print_ltl f =
    List.iter (p_ldecl f)

