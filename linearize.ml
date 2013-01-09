open Ltl
open Register
open Mips

let visited = Hashtbl.create 17
let labels = Hashtbl.create 17

let need_label l = Hashtbl.add labels l ()

let code_output = ref []

let emit lbl instr =
    code_output := (lbl,instr)::!code_output

let string_of_label lbl =
    Format.sprintf "l%d" lbl

let print_mips f =
    let rec insert_labels accu = function
        | [] -> accu
        | (lbl,instr)::t ->
                let ajout = 
                if Hashtbl.mem labels lbl then
                    [(Label (string_of_label lbl)); instr]
                else [instr] in
                insert_labels (ajout @ accu) t
    in
    let print_code f = List.iter (Mips.print_instruction f) in
    print_code f (insert_labels [] !code_output)

let morph = function
  |LLw(r,a,l1)->Lw(r,a)
  |LLb(r,a,l1)->Lb(r,a)
  |LSw(r,a,l1)->Sw(r,a)
  |LSb(r,a,l1)->Sb(r,a)
  |_ -> assert false (* argument invalide *)

let rec lin g lbl =
    if not (Hashtbl.mem visited lbl) then
      begin
          Hashtbl.add visited lbl ();
          instr g lbl (Ltl.find_instr g lbl)
      end
    else
      begin
          need_label lbl;
          emit (Rtl.fresh_label ()) (J (string_of_label lbl))
      end

and instr g lbl instruction =
    match instruction with
    | Lmove(x,y,l1) ->
            if x = y then
              begin
                emit lbl Nop;
                lin g l1
              end
            else
             begin
                emit lbl (Move(x,y));
                lin g l1
             end
    | LLa(r,Alab(a),l1) ->
            emit lbl (La(r,a));
            lin g l1
    | LLa(r,Areg(offset,reg),l1) ->
            emit lbl (Arith(Mips.Add,r,reg,Oimm(Int32.to_int offset)));
            lin g l1
    | LLi(r,i,l1) ->
            emit lbl (Li32(r,i));
            lin g l1
    | LLw(r,a,l1)
    | LLb(r,a,l1)
    | LSw(r,a,l1)
    | LSb(r,a,l1) -> emit lbl (morph instruction); lin g l1
    | LArith(mip,r1,r2,op,l)->
        begin
        match op with
          | Rtl.Oreg(a)->
                  emit lbl (Arith(mip,r1,r2,Oreg(a)));
                  lin g l
          | Rtl.Oimm(i)->
                     if (Int32.to_int i) > 60000 then
                         begin
                            emit lbl (Li32(r1,i));
                            emit (Rtl.fresh_label()) (Arith(mip,r1,r2,Oreg(r1)));
                            lin g l
                         end
                    else
                        begin
                            emit lbl (Arith(mip,r1,r2,Oimm(Int32.to_int i)));
                            lin g l
                        end
        end
    | LSet(mip,r1,r2,op,l)-> 
    begin
        match op with
          | Rtl.Oreg(a)->
                  emit lbl (Set(mip,r1,r2,Oreg(a)));
                  lin g l
          | Rtl.Oimm(i)->
                    if (Int32.to_int i) > 60000 then
                        begin
                        emit lbl (Li32(r1,i));
                        emit (Rtl.fresh_label ()) (Set(mip,r1,r2,Oreg(r1)));
                        lin g l
                        end
                    else
                      begin
                        emit lbl (Set(mip,r1,r2,Oimm(Int32.to_int i)));
                        lin g l
                      end
        end
    | LNeg(r1,r2,l) ->
            emit lbl (Neg(r1,r2));
            lin g l
    | LJr(r)->
	    emit lbl (Jr(r))
    | Lsyscall(l)->
            emit lbl Syscall;
            lin g l
    | LBeq(r1,r2,l1,l2) ->
            if not (Hashtbl.mem visited l1) && Hashtbl.mem visited l2 then
                instr g lbl (LBne(r1,r2,l2,l1))
            else
             begin
                need_label l1;
                emit lbl (Beq(r1,r2,(string_of_label l1)));
                lin g l2;
                lin g l1
             end
    | LBne(r1,r2,l1,l2)->
            if not (Hashtbl.mem visited l1) && Hashtbl.mem visited l2 then
                instr g lbl (LBeq(r1,r2,l2,l1))
            else
             begin
                  need_label l1;
                 emit lbl (Bne(r1,r2,(string_of_label l1)));
                lin g l2;
                lin g l1
             end
    | LBeqz(r,l1,l2) ->
            if not (Hashtbl.mem visited l1) && Hashtbl.mem visited l2 then
                instr g lbl (LBnez(r,l2,l1))
            else
             begin
                need_label l1;
                emit lbl (Beqz(r,(string_of_label l1)));
                lin g l2;
                lin g l1
             end
    | LBnez(r,l1,l2) ->
             if not (Hashtbl.mem visited l1) && Hashtbl.mem visited l2 then
                instr g lbl (LBeqz(r,l2,l1))

            else
             begin
                need_label l1 ;         
                emit lbl (Bnez(r,(string_of_label l1)));
                lin g l2;
                lin g l1
             end

    | Lgoto(l) -> 
            emit lbl Nop;
            lin g l
    | Lcall(s,l)-> emit lbl (Jal("f_"^s));lin g l
    | Lset_stack(r,i,l)->emit lbl (Lw(r,Areg(i,Register.sp))); lin g l
    | Lget_stack(r,i,l)->emit lbl (Sw(r,Areg(i,Register.sp))); lin g l

let add_meta_main () =
    emit (Rtl.fresh_label ()) (Label "main");
    emit (Rtl.fresh_label ()) (Jal "f_main");
    emit (Rtl.fresh_label ()) (Li (V0,17));
    emit (Rtl.fresh_label ()) (Syscall)

let rec compile_code f = function
    | [] -> print_mips f
    | d::t ->
            emit (Rtl.fresh_label ()) (Label (Format.sprintf "f_%s" d.name));
            lin d.g d.entry;
            compile_code f t 


let compile_fichier f prg =
    Format.fprintf f "\t.text\n";
    add_meta_main (); 
    compile_code f prg;
    Format.fprintf f "\t.data\n";
    List.iter (Mips.print_data f) (Data_segment.get_data ());
    Format.fprintf f "\n";


