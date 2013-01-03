open Types
open Ast

type label = int

let notlabel = -1

type pseudoreg = Register.register

type address =
  | Alab of string
  | Areg of int * pseudoreg

type operand =
  | Oimm of int32
  | Oreg of pseudoreg

type instr =
  | Move of pseudoreg * pseudoreg * label
  | Li   of pseudoreg * int32 * label
  | Lw   of pseudoreg * address * label
  | Sw   of pseudoreg * address * label
  | Address of pseudoreg * pseudoreg * label
  | Arith of Mips.arith * pseudoreg * pseudoreg * operand * label
  | Set of Mips.condition * pseudoreg * pseudoreg * operand * label
  | Neg  of pseudoreg * pseudoreg * label
  | B    of label
  | Beq  of pseudoreg * pseudoreg * label * label
  | Beqz of pseudoreg * label * label
  | Bnez of pseudoreg * label * label
  | Return of pseudoreg option * label
  | Call of string * pseudoreg list * pseudoreg * label
  | Putchar of pseudoreg (*argument*) * pseudoreg (*valeur de retour*) * label
  | Sbrk of pseudoreg (*argument*) * pseudoreg (*valeur de retour*) * label

module M = Map.Make(struct type t=label
    let compare = compare end)

type graph = instr M.t

(* Dans Fct, le dernier label est le point d'entrée de la fonction *)
type decl =
   | Fct of pseudoreg (* retval *) * string (* name *) * (pseudoreg list) * graph
  * label (* entry *) * label (* exit *) * Register.set (* locals *)
  | Glob of pseudoreg

(* Gestion des pseudoregistres et des labels *)

let pseudoreg_counter = ref 0

let fresh_pseudoreg () =
    let oldval = !pseudoreg_counter in
    incr pseudoreg_counter;
    Register.Pseudo oldval

let label_counter = ref 0

let fresh_label () =
    let oldval = !label_counter in
    incr label_counter;
    oldval

let max_label () =
    !label_counter

let graph = ref M.empty
let locals = ref Register.Rset.empty

let reset_graph () =
    graph := M.empty;
    locals := Register.Rset.empty;
    pseudoreg_counter := 0

let generate instr =
    let lbl = fresh_label () in
    graph := M.add lbl instr !graph;
    lbl

let add_instr lbl instr =
    graph := M.add lbl instr !graph

let find_instr g lbl =
    M.find lbl g

let iter_instr g fct =
    M.iter fct g

let arith_of_binop = function
   | Ast.AB_plus -> Mips.Add
   | Ast.AB_minus -> Mips.Sub
   | Ast.AB_times -> Mips.Mul
   | Ast.AB_div -> Mips.Div
   | Ast.AB_mod -> Mips.Rem
   | _ -> assert false

let set_of_binop = function
   | Ast.AB_equal -> Mips.Eq
   | Ast.AB_diff -> Mips.Ne
   | Ast.AB_lt -> Mips.Lt
   | Ast.AB_gt -> Mips.Gt
   | Ast.AB_leq -> Mips.Le
   | Ast.AB_geq -> Mips.Ge
   | _ -> assert false

let arith_or_set binop r1 r2 r3 lbl = match binop with
   | Ast.AB_plus
   | Ast.AB_minus
   | Ast.AB_times
   | Ast.AB_div 
   | Ast.AB_mod -> 
        generate (Arith (arith_of_binop binop, r1, r2, r3, lbl))
   | Ast.AB_equal
   | Ast.AB_diff
   | Ast.AB_lt
   | Ast.AB_leq
   | Ast.AB_gt
   | Ast.AB_geq ->
        generate (Set (set_of_binop binop, r1, r2, r3, lbl))
   | Ast.AB_gets -> assert false
   | _ -> assert false (* TODO : and, or *)
(* Évaluation partielle des expressions *)

let rec is_immediate (t,exp) = match exp with
   | TE_int _
   | TE_char _ -> true
   | TE_str _
   | TE_ident _
   | TE_star _
   | TE_dot _
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
    | AB_plus -> Int32.add a b (* TODO Attention à l'arithmétique de pointeur *)
    | AB_minus -> Int32.sub a b
    | AB_times -> Int32.mul a b
    | AB_div -> Int32.div a b
    | AB_mod -> Int32.rem a b
    | AB_and -> int32_of_bool ((bool_of_int32 a) && (bool_of_int32 b))
    | AB_or -> int32_of_bool ((bool_of_int32 a) || (bool_of_int32 b))
    | AB_gets | _ -> assert false

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

let rec compile_affectation env (t,left_value) right_register to_label =
    match left_value with
    | TE_ident name ->
            let pr = Env.find name env in
            generate (Move(right_register,pr,to_label))
    | TE_star e ->
            let pr = fresh_pseudoreg () in
            compile_expr env pr e
            (generate (Sw(right_register,Areg(0,pr),to_label)))     
    | TE_dot (e,field) ->
            let pr = fresh_pseudoreg () in
            compile_expr env pr e
            (generate (Move(right_register,pr,to_label))) 
    | _ -> (* not a left value *) assert false

and compile_args env to_label = function
   | [] -> ([],to_label)
   | t::q ->
           let (regs,from_label) = compile_args env to_label q in
           let target_reg = fresh_pseudoreg () in
           (target_reg::regs,
           compile_expr env target_reg t from_label)

and compile_expr env destreg (t,exp) to_label =
    if t <> ET_int && t <> ET_void && t <> ET_null then
    begin
        Format.printf "Type not implemented : %a@\n" Print_typed_ast.p_ctype t;
        exit 2
    end; (* not implemented *)
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
     | TE_star e ->
             let pr = fresh_pseudoreg () in
             compile_expr env pr e
             (generate (Lw (destreg,Areg(0,pr),to_label)))
     | TE_dot(e,field) ->
             let offset = Sizeof.get_offset t field in
             let pr = fresh_pseudoreg () in
             compile_expr env pr e
             (generate (Lw (destreg,Areg(offset,pr),to_label)))
     | TE_gets(e1,e2) -> 
             let pr = fresh_pseudoreg () in
             compile_expr env pr e2
             (compile_affectation env e1 pr to_label)
     | TE_incr(incr,e) ->
             (match incr with
              | IncrRet
              | DecrRet ->
                      let op = if incr = IncrRet then AB_plus else AB_minus in
                      compile_expr env destreg
         (t,TE_gets(e,(t,TE_binop(op,e,(ET_int,TE_int(Int32.one)))))) to_label
              | RetIncr
              | RetDecr ->
                      let pr = fresh_pseudoreg () in
                      let pr2 = fresh_pseudoreg () in
                      let op = if incr = RetIncr then Mips.Add else Mips.Sub in
                      (compile_expr env pr e
                      (generate (Move(pr,destreg,
                      (generate (Arith(op,pr2,pr,Oimm(Int32.one),
                      compile_affectation env e pr2 to_label))))))))
     | TE_unop(op,e) ->
             (match op with
              | AU_addr ->
                      assert false (* TODO *)
              | AU_not ->
                      let pr = fresh_pseudoreg () in
                      compile_expr env pr e
                      (generate (Neg (destreg,pr,to_label)))
              | AU_minus ->
                      let pr = fresh_pseudoreg () in
                      compile_expr env pr e
                      (generate (Arith(Mips.Sub,destreg,Register.ZERO,Oreg(pr),to_label)))
              | AU_plus -> compile_expr env destreg e to_label)
     | TE_str s -> assert false (* TODO *)
     | TE_char c -> generate (Li(destreg,Int32.of_int (int_of_char c),to_label)))

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
             (arith_or_set binop destreg reg (Oimm operand) to_label)
     | (false,false) ->
             let reg1 = fresh_pseudoreg () in
             let reg2 = fresh_pseudoreg () in
             compile_expr env reg1 a
             (compile_expr env reg2 b
             (arith_or_set binop destreg reg1 (Oreg reg2) to_label))

let compile_condition env (t,expr) true_case false_case = match expr with
    | e when is_immediate (t,e) ->
            let n = compute_immediate e in
            if Int32.compare n Int32.zero = 0 then
                false_case
            else true_case
    | TE_str s -> true_case
    | TE_unop(AU_not,e) ->
            let pr = fresh_pseudoreg () in
            compile_expr env pr e
            (generate (Beqz (pr,true_case,false_case)))
(* TODO : (just to use beq) *)
(*   | TE_binop(AB_equal,a,b) when is_numeric a
                            && is_numeric b -> *)
        (* Beq *)
    | e -> let pr = fresh_pseudoreg () in
           compile_expr env pr (t,expr)
           (generate (Bnez (pr,true_case,false_case)))

let compile_expr_opt env to_label = function
    | None -> to_label
    | Some e -> compile_expr env Register.Notreg e to_label

let add_local env name =
    let pr = fresh_pseudoreg () in
    locals := Register.Rset.add pr !locals;
    Env.add name pr env

(* Compilation des instructions *)
let rec compile_bloc env to_label (decl_vars,instr_list) =
    let nenv = List.fold_left
    (fun env (t,name) -> add_local env name)
    env decl_vars in
    List.fold_left (compile_instr nenv) to_label (List.rev instr_list)

and compile_instr env to_label = function
    | VT_none ->
            to_label
    | VT_inst exp ->
            compile_expr env Register.Notreg exp to_label 
    | VT_return None ->
            generate (Return (None,to_label))
    | VT_return (Some v) ->
            let pr = fresh_pseudoreg () in
            compile_expr env pr v (generate (Return (Some pr, to_label)))
    | VT_if (cond,instr) ->
            compile_condition env cond
                (compile_instr env to_label instr)
                to_label
    | VT_if_else (cond,pos,neg) ->
            compile_condition env cond
                (compile_instr env to_label pos)
                (compile_instr env to_label neg)
    | VT_while (cond,instr) ->
            let lbl = fresh_label () in
            let goto_lbl = compile_condition env cond
                (compile_instr env lbl instr)
                to_label in
            add_instr lbl (B goto_lbl);
            goto_lbl
    | VT_for (expr_list, expr_opt, expr_list_2, instr) ->
           let goto_lbl = fresh_label () in
           let bloc =
              compile_instr env (List.fold_right (compile_expr env Register.Notreg)
              (List.rev expr_list_2) goto_lbl) instr in
           let entry = (match expr_opt with
            | None -> bloc
            | Some a -> compile_condition env a bloc to_label) in
           add_instr goto_lbl (B entry);
           List.fold_right (compile_expr env Register.Notreg)
                (List.rev expr_list) entry 
    | VT_bloc bloc -> compile_bloc env to_label bloc

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
                let g_copy = !graph in
                let loc_copy = !locals in
                (Fct (Register.Notreg, name, reg_args, g_copy, entry, to_label,
                loc_copy))::
                (compile_decl glob_env t)
    in
    compile_decl Env.empty fichier

