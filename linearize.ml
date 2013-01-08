open Ltl
open Register
open Mips

let visited = Hashtbl.create 17
let labels = Hashtbl.create 17

let need_label l = Hashtbl.add labels l ()

let rec lin g lbl =
    if not (Hashtbl.mem visited lbl) then
      begin
          Hashtbl.add visited lbl ();
          instr g lbl (Ltl.find_instr g lbl)
      end
    else
      begin
          need_label lbl;
          emit (Rtl.fresh_label ()) (Lgoto lbl)
      end

and instr g lbl instr =
  match instr with
    | Lmove(x,y,l1)-> if x = y then lin g l1 else [Move(x,y)] ++ (lin g l1)
    | LLi(r,i,l1)->[Li32(r,i)]++(lin g l1)
    | LLa(r,a,l1)->[La(r,a)]++(lin g l1)
    | LLw(r,a,l1)->[Lw(r,a)]++(lin g l1) 
    | LLb(r,a,l1)->[Lb(r,a)]++(lin g l1)
    | LSw(r,a,l1)->[Sw(r,a)]++(lin g l1)
    | LSb(r,a,l1)->[Sb(r,a)]++(lin g l1)
    | LArith(mip,r1,r2,op,l)->
        begin
        match op with
          |Rtl.Oreg(a)->[Arith(mip,r1,r2,Oreg(a))]++(lin g l)
          |Rtl.Oimm(i)->
                     if (Int32.to_int i) > 60000 then
                    [Li32(r1,i);Arith(mip,r1,r2,Oreg(r1))]++(lin g l)
                    else
                    [Arith(mip,r1,r2,Oimm(Int32.to_int i))]++(lin g l)
               end
    | LSet(mip,r1,r2,op,l)-> 
    begin
        match op with
          |Rtl.Oreg(a)->[Set(mip,r1,r2,Oreg(a))]++(lin g l)
          |Rtl.Oimm(i)->
                    if (Int32.to_int i) > 60000 then
                    [Li32(r1,i);Set(mip,r1,r2,Oreg(r1))]++(lin g l)
                    else
                    [Set(mip,r1,r2,Oimm(Int32.to_int i))]++(lin g l)
        end
    | LNeg(r1,r2,l)->[Neg(r1,r2)]++(lin g l) 
    | LJr(r)->[Jr(r)]
    | Lsyscall(l)->[Syscall]::(lin g l)
    | LReturn->[]
    | LBeq(r1,r2,l1,l2)->
    | LBeqz(r,l1,l2)->
    | LBnez(r,l1,l2)->
    | Lgoto(l)->lin g l
  (* TODO *)


