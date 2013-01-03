open Register

let rec prefix n = function
  | [] -> []
  | _ when n = 0 -> []
  | h::t -> h::(prefix (n-1) t)

let rec from_list = function
  | [] -> Rset.empty
  | h::t -> Rset.add h (from_list t)

let use_def = function 
  | Ertl.Ecall (_,n,_) -> (prefix n parameters), caller_saved
  | Ertl.Esyscall _ -> [V0; A0], [V0]
  | Ertl.Ealloc_frame _ -> [], []
  | Ertl.Edelete_frame _ -> [], []
  | Ertl.Eget_stack_param(r,_,_) -> [], [r] 
  | Ertl.Eset_stack_param(r,_,_) -> [r], [] 
  | Ertl.Emove(r1,r2,_) -> [r1], [r2]
  | Ertl.ELi(r,_,_) -> [], [r]
  | Ertl.EStr(r,_,_) -> [], [r]
  | Ertl.ELw(r,_,_) -> [], [r]
  | Ertl.ESw(r,_,_) -> [r], []
  | Ertl.EAddress(r1,_,r2,_) -> [r2], [r1]
  | Ertl.EArith(_,r1,r2,Rtl.Oimm(_),_) -> [r2], [r1]
  | Ertl.ESet(_,r1,r2,Rtl.Oimm(_),_) -> [r2], [r1]
  | Ertl.EArith(_,r1,r2,Rtl.Oreg(r3),_) -> [r2; r3], [r1]
  | Ertl.ESet(_,r1,r2,Rtl.Oreg(r3),_) -> [r2; r3], [r1]
  | Ertl.ENeg(r1,r2,_) -> [r2], [r1]
  | Ertl.Egoto (_) -> [], []
  | Ertl.EBeq(r1,r2,_,_) -> [r1;r2], []
  | Ertl.EBeqz (r,_,_) -> [r], []
  | Ertl.EBnez (r,_,_) -> [r], []
  | Ertl.EJr(r) -> [r], []
  | Ertl.EReturn -> (result::ra::callee_saved), []

module Lmap = Map.Make(struct type t=Rtl.label
    let compare = compare end)

module Lset = Set.Make(struct type t=Rtl.label
    let compare = compare end)

let uses = ref Lmap.empty
let predecesseurs = ref Lmap.empty

let add_pred map new_pred lbl =
    let current_set =
        try
            Lmap.find lbl !map
        with Not_found -> Lset.empty
    in
    map := Lmap.add lbl (Lset.add new_pred current_set) !map

let calcul_predecesseurs g =
    let voisins_pred = ref Lmap.empty in
    Ertl.M.iter (fun lbl instr ->
        let lst = Ertl.successeurs instr in
        List.iter (add_pred voisins_pred lbl) lst) g;
    let rec dfs start =
        try
            Lmap.find start !predecesseurs
        with Not_found ->
            (let preds = Lmap.find start !voisins_pred in
            let accu = ref Lset.empty in
            Lset.iter (fun x -> accu := Lset.union !accu (dfs x))
            preds;
            predecesseurs := Lmap.add start !accu !predecesseurs;
            !accu) 
    in
    Ertl.M.iter (fun lbl instr -> let _ = dfs lbl in ()) g

let get_in_out lbl =
    try
        Lmap.find lbl !uses
    with Not_found -> (Lset.empty,Lset.empty)

let kildall g =
    predecesseurs := Lmap.empty;
    calcul_predecesseurs g;
    let working_list = ref Lset.empty in

    (* On ajoute tous les labels dans la working_list *)
    Ertl.M.iter (fun lbl instr -> working_list := Lset.add lbl !working_list) g; 

    while not (Lset.is_empty !working_list) do
        let lbl = Lset.choose !working_list in
        let (oldin,_) = get_in_out lbl in
        () (* TODO *)
    done

