open Ertl
open Register
open Rtl
open Irc
open Kildall

let print_uses = ref false
let print_colors = ref false

type instr=
  | Lcall of string*label
  | Lsyscall of label
  | Lget_stack of register*int32*label
  | Lset_stack of register*int32*label
  (*Suite ne change pas de précedemment*)
  | Lmove of register*register*label
  | LNeg of register*register*label
  | LLi   of register * int32 * label
  | LLa   of register * address * label
  | LLw   of register * address * label
  | LSw   of register * address * label
  | LLb   of register * address * label
  | LSb   of register * address * label
  | LArith of Mips.arith * register * register * operand * label
  | LSet of Mips.condition * register* register* operand* label
  | Lgoto   of label
  | LBeq  of register * register * label * label
  | LBne  of register * register * label * label
  | LBeqz of register * label * label
  | LBnez of register * label * label
  | LJr   of register

module M = Map.Make(struct type t=label
  let compare = compare end)

(*Fonctions de du graphe et de la frame*)
type graph = instr M.t

let graph = ref M.empty
let frame_stack_param_size = ref 0
let frame_spilled_size = ref 0
let frame_su_size = ref 0
let total_frame_size = ref 0

let get_spilled_address n =
  Int32.of_int
  (!frame_su_size + !frame_spilled_size - 4*(n+1))

let reset_graph () =
  graph := M.empty

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


let tmp1, tmp2 = V1, T7

let write1 c r l = match get_color c r with
  | Reg hr -> hr, l
  | Stack n -> tmp1, generate (Lset_stack (tmp1,
    get_spilled_address n, l))

let read1 c r f = match get_color c r with
  | Reg hr -> f hr
  | Stack n -> Lget_stack (tmp1, get_spilled_address n,
    generate (f tmp1))

let write2 c r l = match get_color c r with
  | Reg hr -> hr, l
  | Stack n -> tmp2, generate (Lset_stack (tmp2,
    get_spilled_address n, l))

let read2 c r f = match get_color c r with
  | Reg hr -> f hr
  | Stack n -> Lget_stack (tmp2,
    get_spilled_address n, generate (f tmp2))

let morph instr a b l = match instr with
  | Ertl.ELw(x,y,z)->LLw(a,b,l)
  | Ertl.ELb(x,y,z)->LLb(a,b,l)
  | Ertl.ELa(x,y,z)->LLa(a,b,l)
  | Ertl.ESb(x,y,z)->LSb(a,b,l)
  | Ertl.ESw(x,y,z)->LSw(a,b,l)
  | _-> assert(false) (*Utilisation non conventionnelle*)

(*Fonction de traduction principale*)
let rec instr c frame_size ins = match ins with
  | Ertl.ELi(r1,i,l) -> let hw,l=write1 c r1 l in LLi(hw,i,l)
  | Ertl.ELw(r,i,l) 
  | Ertl.ELb(r,i,l)
  | Ertl.ELa(r,i,l)-> 
    begin
      match i with
      | Alab(s)->
        let hw,l=write1 c r l in morph ins hw i l
      | Areg(a,p)->let hw,l=write1 c r l in
      read2 c p (fun x->morph ins hw (Areg(a,x)) l)   
  end  
  | Ertl.ESb(r,i,l)
  | Ertl.ESw(r,i,l) ->
    begin
      match i with
      | Alab(s)-> read1 c r (fun x-> morph ins x i l)
      | Areg(a,p)-> read1 c r 
        (fun x-> read2 c p (fun y-> morph ins x (Areg(a,y)) l))
  end
  | Ertl.EBeqz(r,l1,l2)->read1 c r (fun x->LBeqz(x,l1,l2))
  | Ertl.EBnez(r,l1,l2)->read1 c r (fun x->LBnez(x,l1,l2))
  | Ertl.EJr(r)->read1 c r (fun x->LJr(x))
  | Ertl.Einit_addr(r,offset,l) ->
    instr c frame_size (EArith(Mips.Add,r,SP,Oimm(Int32.of_int offset),l))
  | Ertl.EAddress(r1,r2,l)->
    (match get_color c r2 with
    | Reg r -> Format.printf "problème : addresse de %a\n"
               Print_rtl.p_pseudoreg
      r; assert false (* IRC n'a pas fait son boulot ! *)
    | Stack n ->
      instr c frame_size (EArith(Mips.Add,r1,SP,
      Oimm(get_spilled_address n),l)))
  | Ertl.EReturn-> LJr(Register.ra)
  | Ertl.Egoto(l)-> Lgoto(l)
  | Ertl.Ecall(s,i,l)-> Lcall(s,l)
  | Ertl.Esyscall(l)->Lsyscall(l)
  | Ertl.Emove(r1,r2,l) when (Irc.get_color c r1) = (Irc.get_color c r2)
         -> Lgoto(l)
  | Ertl.Emove(r1,r2,l)->
    (match (get_color c r1),(get_color c r2) with
     | Reg(r1),Reg(r2) -> Lmove(r1,r2,l)
     | Reg(r1),Stack(n) ->
        Lset_stack (r1,get_spilled_address n, l)
     | Stack(n),Reg(r2) ->
        Lget_stack (r2,get_spilled_address n, l)
     | Stack(n1),Stack(n2) ->
        Lget_stack (tmp1,get_spilled_address n1,
        generate (Lset_stack(tmp1,get_spilled_address n2,l))))
  | Ertl.ENeg(r1,r2,l)->
    let (hw1,l)=write2 c r1 l in
    read1 c r2 (fun x -> LNeg(hw1,x,l))
  | Ertl.EBeq(r1,r2,l1,l2)->
    read1 c r2 (fun x-> read2 c r1 (fun y-> LBeq(y,x,l1,l2)))  
  | Ertl.EBne(r1,r2,l1,l2)->
    read1 c r2 (fun x-> read2 c r1 (fun y-> LBne(y,x,l1,l2)))  
  | Ertl.ESet(op,r2,r3,operand,l)->
    begin
      match operand with 
      | Rtl.Oimm(a)->
        read1 c r3 
          (fun x-> let (hw1,l)= write1 c r2 l in
          LSet(op,hw1,x,Oimm(a),l)
        )
      | Rtl.Oreg(b)-> 
        let f tmp regist= let (hw1,l)=write1 c r2 l in
        LSet(op,hw1,tmp,Oreg(regist),l) 
        in 
        read1 c r3 (fun x-> (read2 c b (fun y-> f x y)))    
  end
  | Ertl.EArith (op, r2, r3,operand, l) ->
    begin
      match operand with 
      |Oimm(a)->
        read1 c r3 
          (fun x-> let (hw1,l)= write1 c r2 l in
          LArith(op,hw1,x,Oimm(a),l)
        )
      |Oreg(b)-> 
        let f tmp regist= let (hw1,l)=write1 c r2 l in
        LArith(op,hw1,tmp,Oreg(regist),l) 
        in 
        read1 c r3 (fun x-> (read2 c b (fun y-> f x y)))    
  end
  | Ertl.Eget_stack_param (r, n, l) ->
    let hwr, l = write1 c r l in
    LLw(hwr,Areg(Int32.of_int (!total_frame_size - 4*(n+1)),Register.sp),l)
  | Ertl.Eset_stack_param (r,n,l) ->
    read1 c r (fun x -> LSw(x, Areg(Int32.of_int (-4*(n+1)),Register.sp), l))
  | Ertl.Ealloc_frame l
  | Ertl.Edelete_frame l when frame_size = 0 ->
    Lgoto l
  | Ertl.Ealloc_frame l ->
    LArith(Mips.Add, Register.sp,
           Register.sp,Oimm(Int32.of_int(-frame_size)), l)
  | Ertl.Edelete_frame l ->
    LArith(Mips.Add, Register.sp,
           Register.sp,Oimm(Int32.of_int(frame_size)), l)
  | Ertl.ELoop_begin l
  | Ertl.ELoop_end l -> Lgoto l

type decl =
  { name :string; entry :label; g : graph }


(*Traduction ERTL->LTL d'une fonction*)
let deffun d =
  let (uses,statistics) = Kildall.compute_uses_stats d in
  let c = allocate_registers d.Ertl.g uses statistics
  in
  (* Affichage *)
  if !print_uses || !print_colors then
      Format.printf "function %s:\n" d.Ertl.name;
  if !print_uses then
      Print_ertl.with_uses Format.std_formatter (d,uses);
  if !print_colors then
      Irc.print_coloring Format.std_formatter c;

  (* Calcul des tailles de frame *)
  frame_stack_param_size :=
    4*(max 0 (d.nb_args-List.length Register.parameters));
  frame_spilled_size := 4*(Irc.spilled_count c);
  frame_su_size := d.Ertl.su_size;
  total_frame_size := !frame_spilled_size + !frame_stack_param_size +
    !frame_su_size;
  let frame_size = !total_frame_size in

  (* Traduction *)
  graph := M.empty;
  Ertl.M.iter (fun l i ->
    let i = instr c frame_size i in
    graph :=M.add l i !graph)
    d.Ertl.g;

  (* Registres caller-saved inutilisés *)
  Kildall.used_cs_regs :=
      Fmap.add d.Ertl.name (Irc.get_used_cs c) !Kildall.used_cs_regs;

  { name = d.Ertl.name;
    entry = d.Ertl.entry;
    g = !graph }

let rec compile_fichier = function 
  | [] -> []
  | d::t ->
    let res = deffun d in
    res::(compile_fichier t)

