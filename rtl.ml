open Format

type label = int

type pseudoreg = int

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
  | Call of string * pseudoreg list * pseudoreg * label

module M = Map.Make(struct type t=label
    let compare = compare end)

type graph = instr M.t

type local_env = pseudoreg Types.Env.t

let pseudoreg_counter = ref 0

let fresh_pseudoreg () =
    let oldval = !pseudoreg_counter in
    incr pseudoreg_counter;
    oldval

let label_counter = ref 0

let fresh_label () =
    let oldval = !label_counter in
    incr label_counter;
    oldval

let graph = ref M.empty

let generate instr =
    let lbl = fresh_label () in
    graph := M.add lbl instr !graph;
    lbl

let rec compile_args env to_label = function
   | [] -> ([],to_label)
   | t::q ->
           let (regs,from_label) = compile_args env to_label q in
           let target_reg = fresh_pseudoreg () in
           (target_reg::regs,
           compile_expr env target_reg from_label t)

and compile_expr env destreg to_label (t,exp) =
    if t <> Types.ET_int && t <> Types.ET_void then
        assert false; (* not implemented *)
    (match exp with
     | Types.TE_int n ->
              generate (Li (destreg,n,to_label))
     | Types.TE_ident id ->
             (try
                 let pr = Types.Env.find id env in
                 generate (Move (pr,destreg,to_label))
             with Not_found ->
                 assert false)
     | Types.TE_call (name,args) ->
             let inter_lbl = fresh_label () in
             let (args_list,from_label) = compile_args env inter_lbl args in
             graph := M.add inter_lbl (Call (name,args_list,destreg,to_label))
             !graph;
             from_label
     | _ -> assert false)
                
let compile_instr env to_label = function
    | Types.VT_none ->
            to_label
    | Types.VT_inst exp ->
            compile_expr !env (fresh_pseudoreg ()) to_label exp
            (* TODO : replace fresh_pseudoreg with Notreg *)
    | Types.VT_return _ ->
            to_label (* TODO *)
    | _ -> assert false


     (* TODO : handle decl_vars *)
let compile_bloc env to_label (decl_vars,instr_list) =
    List.fold_left (compile_instr env) to_label (List.rev instr_list)

let compile_fichier fichier =
    graph := M.empty;
    let to_label = fresh_label () in
    let rec compile_decl = function
        | [] -> ()
        | Types.Tdecl_vars(_)::t -> assert false
        | Types.Tdecl_typ(_)::t -> assert false
        | Types.Tdecl_fct (ret_type,name, args, body)::t ->
                let env = ref Types.Env.empty in
                let _ = compile_bloc env to_label body in
                compile_decl t
    in
    compile_decl fichier;
    !graph

let p_label f = fprintf f "L%d"

let p_pseudoreg f n = fprintf f "%%%d" n 

let p_address f = function
    | Alab s -> fprintf f "%s" s
    | Areg (n,pr) -> fprintf f "%d(%a)" n p_pseudoreg pr

let p_operand f = function
    | Oimm n -> fprintf f "%d" n
    | Oreg pr -> p_pseudoreg f pr

let rec p_list sep printer f = function
    | [] -> ()
    | [h] -> printer f h
    | h::t -> fprintf f "%a%s%a"
              printer h
              sep
              (p_list sep printer) t

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
    | Call (name,args,destreg,lbl) -> fprintf f "%a := %s(%a)\t\t-> %a"
        p_pseudoreg destreg name (p_list "," p_pseudoreg) args p_label lbl

let print_rtl f g =
    let p_binding l i =
       fprintf f "%a : %a\n" p_label l p_instr i in
    M.iter p_binding g

