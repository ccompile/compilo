open Ertl
open Register
open Rtl
open Irc

type instr=
  | Lcall of string*int*label
  | Lsyscall of label
  | Lalloc_frame of label
  | Ldelete_frame of label
  | Lget_stack of register*int*label
  | Lset_stack of register*int*label
(*Suite ne change pas de précedemment*)
  | Lmove of register*register*label
  | LLi   of register * int32 * label
  | LStr   of register * string * label
  | LLw   of register * address * label
  | LSw   of register * address * label
  | LLb   of register * address * label
  | LSb   of register * address * label
  | LAddress of register * int * register * label
  | LArith of Mips.arith * register * register * operand * label
  | LSet of Mips.condition * register* register* operand* label
  | Lgoto   of label
  | LBeq  of register * register * label * label
  | LBeqz of register * label * label
  | LBnez of register * label * label
  | LJr   of register
  | LReturn  

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
let lookup c r=  Reg(A0)

let write1 c r l = match lookup c r with
  | Reg hr -> hr, l
  | Stack n -> tmp1, generate (Lset_stack (tmp1, n, l))

let read1 c r f = match lookup c r with
  | Reg hr -> f hr
  | Stack n -> Lget_stack (tmp1, n, generate (f tmp1))

let write2 c r l = match lookup c r with
  | Reg hr -> hr, l
  | Stack n -> tmp2, generate (Lset_stack (tmp2, n, l))

let read2 c r f = match lookup c r with
  | Reg hr -> f hr
  | Stack n -> Lget_stack (tmp2, n, generate (f tmp2))


let instr c frame_size = function
(*REGROUPEMENT en factorisation possibles futures facilement*)

(*  | Ertl.Econst (r, n, l) ->
      let hwr, l = write1 c r l in
      Econst (hwr, n, l)
*)

  | Ertl.ELi(r1,i,l) ->let hw,l=write1 c r1 l in LLi(hw,i,l)
  | Ertl.ELw(r,s,l)->let hw,l =write1 c r l in LLw(hw,s,l)
  | Ertl.ELb(r,ad,l)->let hw,l = write1 c r l in LLb(hw,ad,l)

  | Ertl.ESb(r,ad,l)->read1 c r (fun x-> LSb(x,ad,l))
  | Ertl.ESw(r,ad,l)->read1 c r (fun x-> LSw(x,ad,l))
  | Ertl.EStr(r1,s,l) ->read1 c r1 (fun x->LStr(x,s,l))
  | Ertl.EBeqz(r,l1,l2)->read1 c r (fun x->LBeqz(x,l1,l2))
  | Ertl.EBnez(r,l1,l2)->read1 c r (fun x->LBnez(x,l1,l2))
  | Ertl.EJr(r)->read1 c r (fun x->LJr(x))
 


  | Ertl.EAddress(r1,i,r2,l)->
(*TODO A QUOI SERT CE BIDULE :p j'ai mis n'imp vu que jsavaispas*)
  LAddress(A0,i,A0,l)

  | Ertl.EReturn-> LReturn
  | Ertl.Egoto(l)-> Lgoto(l)
  | Ertl.Ecall(s,i,l)-> Lcall(s,i,l)
  | Ertl.Esyscall(l)->Lsyscall(l)

  | Ertl.Emove(r1,r2,l)->
           let (hw1,l)=write2 c r1 l in 
                        read1 c r2 (fun x-> Lmove(hw1,x,l)) 
  

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
    Lget_stack (hwr, frame_size + n, l)

  | Ertl.Eset_stack_param (r, n, l) ->
    read1 c r (fun hwr -> Lset_stack (hwr, n, l))
 
  | Ertl.Ealloc_frame l
  | Ertl.Edelete_frame l when frame_size = 0 ->
    Lgoto l
 
  | Ertl.Ealloc_frame l ->
 LArith(Mips.Add, Register.sp, Register.sp,Oimm(Int32.of_int(-frame_size)), l)

  | Ertl.Edelete_frame l ->
 LArith(Mips.Add, Register.sp, Register.sp,Oimm(Int32.of_int(frame_size)), l)

  |_->assert(false)
(*
let deffun t =
  (*let ln = Liveness.analyze f.Ertl.fun_body in
  let ig = Interference.make ln in
  let c, nlocals = Coloring.find ig in
   CECI PERMET DE GENERER TOUT JUSQU'AU COLORIAGE*)
  let n stack params =
    max 0 (f.Ertl.fun_formals-List.length Register.parameters)
  in
  let frame size = word size * (nlocals + n stack params) in
  graph := Label.M.empty;
  Label.M.iter (fun l i ->
    let i = instr c frame size i in
    graph := Label.M.add l i !graph)
    f.Ertl.fun_body;
  { fun name = f.Ertl.fun_name;
    fun entry = f.Ertl.fun_entry;
    fun body = !graph; }
*)