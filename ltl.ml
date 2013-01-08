open Ertl
open Register
open Rtl
open Irc
open Kildall

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


type graph = instr M.t

let graph = ref M.empty

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
  | Stack n -> tmp1, generate (Lset_stack (tmp1, Int32.of_int n, l))

let read1 c r f = match get_color c r with
  | Reg hr -> f hr
  | Stack n -> Lget_stack (tmp1,Int32.of_int n, generate (f tmp1))

let write2 c r l = match get_color c r with
  | Reg hr -> hr, l
  | Stack n -> tmp2, generate (Lset_stack (tmp2,Int32.of_int n, l))

let read2 c r f = match get_color c r with
  | Reg hr -> f hr
  | Stack n -> Lget_stack (tmp2,Int32.of_int n, generate (f tmp2))


let instr c frame_size = function
(*REGROUPEMENT en factorisation possibles futures facilement*)
  | Ertl.ELi(r1,i,l) -> let hw,l=write1 c r1 l in LLi(hw,i,l)
  
 
  | Ertl.ELw(r,i,l)-> 
  begin
  match i with
    | Alab(s)->
          let hw,l=write1 c r l in LLw(hw,i,l)
    | Areg(a,p)->let hw,l=write1 c r l in
       read2 c p (fun x->LLw(hw,Areg(a,x),l))   
  end  
  | Ertl.ELb(r,i,l)->
  begin
  match i with
    | Alab(s)->
          let hw,l=write1 c r l in LLb(hw,i,l)
    | Areg(a,p)->let hw,l=write1 c r l in
       read2 c p (fun x->LLb(hw,Areg(a,x),l))   
  end 
  | Ertl.ELa(r,i,l)-> 
  begin
  match i with
    | Alab(s)->
          let hw,l=write1 c r l in LLa(hw,i,l)
    | Areg(a,p)->let hw,l=write1 c r l in
       read2 c p (fun x->LLa(hw,Areg(a,x),l))   
  end  

 
  | Ertl.ESb(r,i,l)->
  begin
  match i with
    | Alab(s)-> read1 c r (fun x-> LSb(x,i,l))
    |Areg(a,p)-> read1 c r 
        (fun x-> read2 c p (fun y-> LSb(x,Areg(a,y),l)))
  end
  | Ertl.ESw(r,i,l) ->
  begin
  match i with
    | Alab(s)-> read1 c r (fun x-> LSw(x,i,l))
    |Areg(a,p)-> read1 c r 
        (fun x-> read2 c p (fun y-> LSw(x,Areg(a,y),l)))
  end

  
  | Ertl.EBeqz(r,l1,l2)->read1 c r (fun x->LBeqz(x,l1,l2))
  | Ertl.EBnez(r,l1,l2)->read1 c r (fun x->LBnez(x,l1,l2))
  | Ertl.EJr(r)->read1 c r (fun x->LJr(x))
 


  | Ertl.EAddress(r1,r2,l)->
    (match get_color c r2 with
     | Reg r -> Format.printf "problème : addresse de %a\n" Print_rtl.p_pseudoreg
     r; assert false (* IRC n'a pas fait son boulot ! *)
     | Stack n -> LArith(Mips.Add,r1,SP,Oimm(Int32.of_int n),l))

  | Ertl.EReturn-> LJr(Register.ra)
  | Ertl.Egoto(l)-> Lgoto(l)
  | Ertl.Ecall(s,i,l)-> Lcall(s,l)
  | Ertl.Esyscall(l)->Lsyscall(l)

  | Ertl.Emove(r1,r2,l)->
           let (hw1,l)=write2 c r1 l in 
                        read1 c r2 (fun x-> Lmove(hw1,x,l)) 
  
  | Ertl.ENeg(r1,r2,l)->
          let (hw1,l)=write2 c r1 l in
              read1 c r2 (fun x -> LNeg(hw1,x,l))
  | Ertl.EBeq(r1,r2,l1,l2)->
        read1 c r2 (fun x-> read2 c r1 (fun y-> LBeq(y,x,l1,l2)))  

 
 
  | Ertl.ESet(op,r2,r3,operand,l)->
  begin
      match operand with 
        |Rtl.Oimm(a)->
          read1 c r3 
                (fun x-> let (hw1,l)= write1 c r2 l in
                      LSet(op,hw1,x,Oimm(a),l)
                )
        |Rtl.Oreg(b)-> 
          let f tmp regist= let (hw1,l)=write1 c r2 l in
          LSet(op,hw1,tmp,Oreg(regist),l) 
          in 
          read1 c r3 (fun x-> (read2 c b (fun y-> f x y)))    
      end
  | Ertl.EArith ( op, r2, r3,operand, l) ->
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
    LLw(hwr,Areg(Int32.of_int n,Register.sp),l)

  | Ertl.Eset_stack_param (r, n, l) ->
    read1 c r (fun x -> LSw(x, Areg(Int32.of_int n,Register.sp), l))
 
  | Ertl.Ealloc_frame l
  | Ertl.Edelete_frame l when frame_size = 0 ->
    Lgoto l
 
  | Ertl.Ealloc_frame l ->
 LArith(Mips.Add, Register.sp, Register.sp,Oimm(Int32.of_int(-frame_size)), l)

  | Ertl.Edelete_frame l ->
 LArith(Mips.Add, Register.sp, Register.sp,Oimm(Int32.of_int(frame_size)), l)

type decl =
    { name :string; entry :label; g : graph }

let deffun d =
  
  let c = allocate_registers d.Kildall.g d.Kildall.uses in
  let loc = Irc.spilled_count c in
  (*let ln = Liveness.analyze f.Ertl.fun_body in
  let ig = Interference.make ln in
  let c, nlocals = Coloring.find ig in
   CECI PERMET DE GENERER TOUT JUSQU'AU COLORIAGE*)
  let n_stack_params =
    max 0 (d.nb_args-List.length Register.parameters)
  in
  let frame_size = (4 * (loc + n_stack_params)) in
  graph := M.empty;
  Ertl.M.iter (fun l i ->
    let i = instr c frame_size i in
    graph :=M.add l i !graph)
    d.Kildall.g;
  { name = d.Kildall.name;
    entry = d.Kildall.entry;
    g = !graph }

let rec compile_fichier = function 
    |[]->[]
    |d::t->
        (deffun d)::(compile_fichier t)
   
