
let visited = Hashtbl.create 17
let labels = Hashtbl.create 17

let need_label l = Hashtbl.add labels l ()

let lin g lbl =
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

let instr g lbl instr =
    (* TODO *)


