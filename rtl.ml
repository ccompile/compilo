open Format
open Types
open Ast

type label = int

type pseudoreg = int

type address =
  | Alab of string
  | Areg of int * pseudoreg

type operand =
  | Oimm of int32
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
  | Return of pseudoreg
  | Call of string * pseudoreg list * pseudoreg * label
  | Putchar of pseudoreg (*argument*) * pseudoreg (*valeur de retour*) * label
  | Sbrk of pseudoreg (*argument*) * pseudoreg (*valeur de retour*) * label

module M = Map.Make(struct type t=label
    let compare = compare end)

type graph = instr M.t

(* Dans Fct, le dernier label est le point d'entrée de la fonction *)
type decl =
  | Fct of Types.expr_type * string * (pseudoreg list) * graph * label
  | Glob of pseudoreg


type local_env = pseudoreg Env.t

(* Gestion des pseudoregistres et des labels *)

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

let reset_graph () =
    graph := M.empty;
    pseudoreg_counter := 0

let generate instr =
    let lbl = fresh_label () in
    graph := M.add lbl instr !graph;
    lbl
    
let arith_of_binop = function
   | Ast.AB_plus -> Mips.Add
   | Ast.AB_minus -> Mips.Sub
   | Ast.AB_times -> Mips.Mul
   | Ast.AB_div -> Mips.Div
   | Ast.AB_mod -> Mips.Rem
   | _ -> assert false

(* Évaluation partielle des expressions *)

let rec is_immediate (t,exp) = match exp with
   | TE_int _
   | TE_char _ -> true
   | TE_str _
   | TE_ident _
   | TE_star _
   | TE_dot _
   | TE_brackets _
   | TE_arrow _
   | TE_gets _
   | TE_call _
   | TE_incr _ -> false
   | TE_unop (_,e) -> is_immediate e
   | TE_binop (_,a,b) ->
           is_immediate a && is_immediate b
   
let compare_int32 a b op = 
    let comp = Int32.compare a b in
    (match op with
    | AB_equal -> comp = 0
    | AB_diff -> comp <> 0
    | AB_lt -> comp < 0
    | AB_leq -> comp <= 0
    | AB_gt -> comp > 0
    | AB_geq -> comp >= 0
    | _ -> assert false)

let bool_of_int32 a =
    Int32.compare a Int32.zero <> 0

let int32_of_bool a =
    if a then Int32.one else Int32.zero

let arith_int32 a b = function
    | AB_plus -> Int32.add a b
    | AB_minus -> Int32.sub a b
    | AB_times -> Int32.mul a b
    | AB_div -> Int32.div a b
    | AB_mod -> Int32.rem a b
    | AB_and -> int32_of_bool ((bool_of_int32 a) && (bool_of_int32 b))
    | AB_or -> int32_of_bool ((bool_of_int32 a) || (bool_of_int32 b))
    | _ -> assert false

let rec compute_immediate = function
   | TE_int n -> n
   | TE_char c -> Int32.of_int (int_of_char c)
   | TE_unop(AU_minus,(t,e)) -> Int32.neg (compute_immediate e)
   | TE_unop(AU_plus,(t,e)) -> compute_immediate e
   | TE_unop(AU_not,(t,e)) ->
           if Int32.compare (compute_immediate e) Int32.zero = 0 then
               Int32.one
           else Int32.zero
   | TE_binop(comp,(t,a),(t',b))
     when List.mem comp [ AB_equal; AB_diff; AB_lt; AB_leq; AB_gt; AB_geq ] ->
           if compare_int32 (compute_immediate a) (compute_immediate b) comp then
               Int32.one
           else Int32.zero
   | TE_binop(AB_gets,_,_) -> assert false (* expression non immédiate *)
   | TE_binop(binop,(t,a),(t',b)) -> arith_int32 (compute_immediate a)
            (compute_immediate b) binop
   | _ -> assert false 

(* Compilation des expressions *)

let rec compile_args env to_label = function
   | [] -> ([],to_label)
   | t::q ->
           let (regs,from_label) = compile_args env to_label q in
           let target_reg = fresh_pseudoreg () in
           (target_reg::regs,
           compile_expr env target_reg t from_label)

and compile_expr env destreg (t,exp) to_label =
    if t <> ET_int && t <> ET_void then
        assert false; (* not implemented *)
    (match exp with
     | TE_int n ->
              generate (Li (destreg,n,to_label))
     | TE_ident id ->
             (try
                 let pr = Env.find id env in
                 generate (Move (pr,destreg,to_label))
             with Not_found ->
                 assert false)
     | TE_call ("putchar",[arg]) ->
             let inter_reg = fresh_pseudoreg () in
             compile_expr env inter_reg arg 
             (generate (Putchar(inter_reg, destreg, to_label)))
     | TE_call ("sbrk",[arg]) ->
             let inter_reg = fresh_pseudoreg () in
             compile_expr env inter_reg arg
             (generate (Sbrk(inter_reg, destreg, to_label)))
     | TE_call (name,args) ->
             let inter_lbl = fresh_label () in
             let (args_list,from_label) = compile_args env inter_lbl args in
             graph := M.add inter_lbl (Call (name,args_list,destreg,to_label))
             !graph;
             from_label
     | TE_binop(binop,a,b) ->
             compile_binop env destreg to_label binop a b
     | _ -> assert false)

and compile_binop env destreg to_label binop a b =
    match (is_immediate a,is_immediate b) with
     | (true,true) ->
             let value = compute_immediate (TE_binop(binop,a,b)) in
             generate (Li (destreg,value,to_label))
     | (true,false) -> compile_binop env destreg to_label binop b a
     | (false,true) ->
             let operand = compute_immediate (snd b) in
             let reg = fresh_pseudoreg () in
             compile_expr env reg a
             (generate (Arith (arith_of_binop binop, destreg, reg, (Oimm
             operand), to_label)))
     | (false,false) ->
             let reg1 = fresh_pseudoreg () in
             let reg2 = fresh_pseudoreg () in
             compile_expr env reg1 a
             (compile_expr env reg2 b
             (generate (Arith (arith_of_binop binop, destreg, reg1, (Oreg reg2), to_label))))
                
(* Compilation des instructions *)

let compile_instr env to_label = function
    | VT_none ->
            to_label
    | VT_inst exp ->
            compile_expr env (fresh_pseudoreg ()) exp to_label 
    | VT_return _ ->
            to_label (* TODO *)
    | _ -> assert false

     (* TODO : handle decl_vars *)
let compile_bloc env to_label (decl_vars,instr_list) =
    let nenv = List.fold_left
    (fun env (t,name) -> Env.add name (fresh_pseudoreg ()) env)
    env decl_vars in
    List.fold_left (compile_instr nenv) to_label (List.rev instr_list)

let compile_tident (env,lst) (t,n) =
    let pr = fresh_pseudoreg () in
    (Env.add n pr env, pr::lst)

let compile_tident_list env lst =
    let (env,lst) = List.fold_left compile_tident (env,[]) lst in
    (env,List.rev lst)

let compile_fichier fichier =
    reset_graph ();
    let rec compile_decl glob_env = function
        | [] -> [] 
        | Tdecl_vars(lst)::t ->
                let (env,regs) = compile_tident_list glob_env lst in
                let regs2 = List.map (fun reg -> Glob reg) regs in
                regs2 @ (compile_decl env t) 
        | Tdecl_typ(_)::t -> compile_decl glob_env t 
        | Tdecl_fct (ret_type,name, args, body)::t ->
                reset_graph ();
                let to_label = fresh_label () in
                let (env,reg_args) = compile_tident_list glob_env args in
                let entry = compile_bloc env to_label body in
                (Fct (ret_type, name, reg_args, !graph, entry)):: 
                (compile_decl glob_env t)
    in
    compile_decl Env.empty fichier

(* Affichage *)

let p_label f = fprintf f "L%d"

let p_pseudoreg f n = fprintf f "%%%d" n 

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
    | Li(r,n,l) -> fprintf f "li\t%a\t%d\t\t-> %a"
      p_pseudoreg r (Int32.to_int n) p_label l
    | La (r,l1,l2) -> fprintf f "la\t%a\t%a\t\t-> %a"
      p_pseudoreg r p_label l1 p_label l2
    | Lw(r,a,l) -> fprintf f "lw\t%a\t%a\t\t-> %a"
      p_pseudoreg r p_address a p_label l
    | Sw(r,a,l) -> fprintf f "sw\t%a\t%a\t\t-> %a"
      p_pseudoreg r p_address a p_label l
    | Arith(ar,r1,r2,op,l) -> fprintf f "%a %a\t%a\t%a\t-> %a"
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
    | Return(r) -> fprintf f "return\t%a"
      p_pseudoreg r
    | Call (name,args,destreg,lbl) -> fprintf f "%a := %s(%a)\t\t-> %a"
        p_pseudoreg destreg name (p_list "," p_pseudoreg) args p_label lbl
    | Putchar(arg,retval,lbl) -> fprintf f "%a := putchar(%a)\t\t -> %a"
        p_pseudoreg retval p_pseudoreg arg p_label lbl
    | Sbrk(arg,retval,lbl) -> fprintf f "%a := sbrk(%a)\t\t -> %a"
        p_pseudoreg retval p_pseudoreg arg p_label lbl

let print_rtl f g =
    let p_binding l i =
       fprintf f "%a : %a\n" p_label l p_instr i in
    M.iter p_binding g

