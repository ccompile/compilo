open Format
open Ertl
open Register
open Print_rtl

let p_einstr f = function
    | Emove(r1,r2,l) -> fprintf f "move %a %a -> %a"
      Print_rtl.p_pseudoreg r1 p_pseudoreg r2 p_label l
    | ELi(r,n,l) -> fprintf f "li %a %d -> %a"
      p_pseudoreg r (Int32.to_int n) p_label l
    | EStr(r,s,l) -> fprintf f "str %a \"%s\" -> %a"
      p_pseudoreg r s p_label l
    | ELw(r,a,l) -> fprintf f "lw %a %a -> %a"
      p_pseudoreg r p_address a p_label l
    | ESw(r,a,l) -> fprintf f "sw %a %a -> %a"
      p_pseudoreg r p_address a p_label l
    | EAddress(pr1,offset,pr2,l) -> fprintf f "addr %a %d(%a) -> %a"
      p_pseudoreg pr1 offset p_pseudoreg pr2 p_label l
    | EArith(ar,r1,r2,op,l) -> fprintf f "%a %a %a %a -> %a"
      Mips.print_arith ar p_pseudoreg r1 p_pseudoreg r2 p_operand op p_label l
    | ESet(cond,r1,r2,op,l) -> fprintf f "%a %a %a %a -> %a"
      Mips.print_condition cond p_pseudoreg r1 p_pseudoreg r2 p_operand op p_label l
    | ENeg(r1,r2,l) -> fprintf f "neg %a %a -> %a"
      p_pseudoreg r1 p_pseudoreg r2 p_label l
    | Egoto(l) -> fprintf f "goto -> %a"
      p_label l
    | EBeq(r1,r2,l1,l2) -> fprintf f "beq %a %a %a -> %a"
      p_pseudoreg r1 p_pseudoreg r2 p_label l1 p_label l2
    | EBeqz(r,l1,l2) -> fprintf f "beqz %a %a -> %a"
      p_pseudoreg r p_label l1 p_label l2
    | EBnez(r,l1,l2) -> fprintf f "bnez %a %a -> %a"
      p_pseudoreg r p_label l1 p_label l2
    | EReturn -> fprintf f "return"
    | EJr r -> fprintf f "jr %a" p_pseudoreg r
    | Ecall (name,n,lbl) -> fprintf f "call(%s,%d) -> %a"
      name n p_label lbl
    | Esyscall l -> fprintf f "syscall -> %a"
      p_label l
    | Ealloc_frame l -> fprintf f "alloc_frame -> %a"
      p_label l
    | Edelete_frame l -> fprintf f "delete_frame -> %a"
      p_label l
    | Eget_stack_param(r,n,l) -> fprintf f "get_stack_param %a %d -> %a"
      p_pseudoreg r n p_label l
    | Eset_stack_param(r,n,l) -> fprintf f "set_stack_param %a %d -> %a"
      p_pseudoreg r n p_label l

let p_rset f s =
    Print_rtl.p_list "," Print_rtl.p_pseudoreg f (Register.Rset.elements s)

let rec generic_dfs printer dejavu g f start =
   try
       if not (dejavu.(start)) then
      begin   
           let instr = find_instr g start in
           printer start instr;
           dejavu.(start) <- true;
           List.iter (generic_dfs printer dejavu g f) (successeurs instr)
       end
   with Not_found -> ()

let rec ertl_dfs dejavu g f =
    let printer start instr =
        fprintf f "%a : %a\n" p_label start p_einstr instr
    in
    generic_dfs printer dejavu g f 

let current_uses = ref Kildall.Lmap.empty

let rec ertl_with_uses_dfs dejavu g f=
    let printer start instr =
        fprintf f "%a : %a\tin : %a\tout : %a\n"
        p_label start p_einstr instr p_rset (Kildall.get_in !current_uses start)
        p_rset (Kildall.get_out !current_uses start)
    in
    generic_dfs printer dejavu g f

let p_edecl f = function
    | EFct(name,nbargs,g,entry,locals) ->
        let dejavu = Array.make (Rtl.max_label ()) false in 
        fprintf f "%s(%d):\n%a\n\n"
            name nbargs 
            (ertl_dfs dejavu g) entry 
    | EGlob pr ->
         fprintf f "Global : %a\n\n" p_pseudoreg pr

let p_with_uses f = function
    | Kildall.Fct(name,nbargs,g,entry,locals,uses) ->
         let dejavu = Array.make (Rtl.max_label ()) false in
         current_uses := uses;
         fprintf f "%s(%d):\n%a\n"
            name nbargs
         (ertl_with_uses_dfs dejavu g) entry
    | Kildall.Glob pr ->
         fprintf f "Global : %a\n" p_pseudoreg pr

let print_ertl f =
    List.iter (p_edecl f)

let with_uses f =
    List.iter (p_with_uses f) 

