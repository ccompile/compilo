open Format

type label = int

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
  | Beq  of pseudoreg * pseudoreg * label * label
  | Beqz of pseudoreg * label * label
  | Bnez of pseudoreg * label * label
  | Jr   of pseudoreg * label
  | Syscall of label

module M = Map.Make(struct type t=label
    let compare = compare end)

type graph = instr M.t

type local_env = pseudoreg Types.Env.t

let pseudoreg_counter = ref 0

let fresh_pseudoreg () =
    let oldval = !pseudoreg_counter in
    incr pseudoreg_counter;
    Pseudo oldval

let label_counter = ref 0

let fresh_label () =
    let oldval = !label_counter in
    incr label_counter;
    oldval

let add_instr g lbl i =
    g := M.add lbl i !g

let rec compile_expr g env destreg from_label to_label (t,exp) =
    if t <> Types.ET_int && t <> Types.ET_void then
        assert false; (* not implemented *)
    (match exp with
     | Types.TE_int n ->
             add_instr g from_label (Li (destreg,n,to_label))
     | Types.TE_ident id ->
             (try
                 let pr = Types.Env.find id env in
                 add_instr g from_label (Move (pr,destreg,to_label))
             with Not_found ->
                 assert false)
     | Types.TE_call ("putchar",[te]) ->
             let lbl = fresh_label () in
             compile_expr g env A0 from_label lbl te;
             let lbl2 = fresh_label () in
             add_instr g lbl (Li (V0, Int32.of_int 11,lbl2));
             add_instr g lbl2 (Syscall to_label)
     | _ -> assert false)
                
let compile_instr g env from_label to_label = function
    | Types.VT_none ->
            add_instr g from_label (B to_label)
    | Types.VT_inst exp ->
            compile_expr g !env (fresh_pseudoreg ()) from_label to_label exp
            (* TODO : replace fresh_pseudoreg with Notreg *)
    | Types.VT_return _ ->
            add_instr g from_label (B to_label)
    | _ -> assert false


     (* TODO : handle decl_vars *)
let compile_bloc g env from_label to_label (decl_vars,instr_list) =
    let nb_instr = List.length instr_list in
    let _ = List.fold_left
            (fun (lbl,rg) instr ->
                let lbl2 = (if rg < nb_instr then fresh_label () else to_label ) in
                compile_instr g env from_label to_label instr;
                (lbl2,rg+1))
            (from_label,1) instr_list in ()

let compile_fichier fichier =
    let g = ref M.empty in
    let from_label = fresh_label () in
    let to_label = fresh_label () in
    let rec compile_decl = function
        | [] -> ()
        | Types.Tdecl_vars(_)::t -> assert false
        | Types.Tdecl_typ(_)::t -> assert false
        | Types.Tdecl_fct (ret_type,name, args, body)::t ->
                let env = ref Types.Env.empty in
                compile_bloc g env from_label to_label body;
                compile_decl t
    in
    compile_decl fichier;
    !g

let p_label f = fprintf f "L%d"

let p_pseudoreg f = function
    | Notreg -> fprintf f "$x"
    | V0     -> fprintf f "$v0"
    | A0     -> fprintf f "$a0"
    | Pseudo n -> fprintf f "%%%d" n 

let p_address f = function
    | Alab s -> fprintf f "%s" s
    | Areg (n,pr) -> fprintf f "%d(%a)" n p_pseudoreg pr

let p_operand f = function
    | Oimm n -> fprintf f "%d" n
    | Oreg pr -> p_pseudoreg f pr

let p_instr f = function
    | Move(r1,r2,l) -> fprintf f "move\t%a\t%a\t\t-> %a"
      p_pseudoreg r1 p_pseudoreg r2 p_label l
    | Li(r,n,l) -> fprintf f "li\t%a\t%d\t\t-> %a"
      p_pseudoreg r (Int32.to_int n) p_label l
    | La (r,l1,l2) -> fprintf f "la\t%a\t%a\t\t-> %a"
      p_pseudoreg r p_label l1 p_label l2
    | Lw(r,a,l) -> fprintf f "lw\t%a\t%a\t\t-> %a"
      p_pseudoreg r p_address a p_label l
    | Sw(r,a,l) -> fprintf f "sw\t%a\t%a\t\t-> %a"
      p_pseudoreg r p_address a p_label l
    | Arith(ar,r1,r2,op,l) -> fprintf f "%a\t%a\t%a\t%a\t-> %a"
      Mips.print_arith ar p_pseudoreg r1 p_pseudoreg r2 p_operand op p_label l
    | Neg(r1,r2,l) -> fprintf f "neg\t%a\t%a\t\t-> %a"
      p_pseudoreg r1 p_pseudoreg r2 p_label l
    | B(l) -> fprintf f "b\t\t\t\t-> %a"
      p_label l
    | Beq(r1,r2,l1,l2) -> fprintf f "beq\t%a\t%a\t%a\t-> %a"
      p_pseudoreg r1 p_pseudoreg r2 p_label l1 p_label l2
    | Beqz(r,l1,l2) -> fprintf f "beqz\t%a\t%a\t\t-> %a"
      p_pseudoreg r p_label l1 p_label l2
    | Bnez(r,l1,l2) -> fprintf f "bnez\t%a\t%a\t\t-> %a"
      p_pseudoreg r p_label l1 p_label l2
    | Jr(r,l) -> fprintf f "jr\t%a\t\t\t-> %a"
      p_pseudoreg r p_label l
    | Syscall l -> fprintf f "syscall\t\t\t-> %a" p_label l

let print_rtl f g =
    let p_binding l i =
       fprintf f "%a : %a\n" p_label l p_instr i in
    M.iter p_binding g  

