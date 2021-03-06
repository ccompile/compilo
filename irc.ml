open Register

let print_graph_dot = ref false

module Rmap = Map.Make(struct type t=register let compare= compare end)

let map_image map =
    let res = ref Rset.empty in
    Rmap.iter
        (fun _ y -> res := Rset.add y !res)
    map;
    !res

module Mset = Set.Make(struct type t = (register*register)
  let compare = compare end)

type color =
  | Reg of register
  | Stack of int

type coloring =
    { nb_spilled : int;
      colors : color Rmap.t; 
      used_cs : register list }
    (* used_cs : registres caller-saved utilisés par le coloriage *)

let max_deg = List.length available_registers

let available_colors = Register.from_list available_registers

let find_or_empty key map =
  try
    Rmap.find key map
  with Not_found -> Rset.empty

let find_or_empty_mset key map =
  try
    Rmap.find key map
  with Not_found -> Mset.empty

let uses_statistics = ref Kildall.Rmap.empty

let precolored = ref Rset.empty
let prespilled = ref Rset.empty
let initial = ref Rset.empty
let simplify_worklist = ref Rset.empty
let freeze_worklist = ref Rset.empty
let spill_worklist = ref Rset.empty
let spilled_nodes = ref Rset.empty
let coalesced_nodes = ref Rset.empty
let colored_nodes = ref Rset.empty
let select_stack = ref []

let coalesced_moves = ref Mset.empty
let constrained_moves = ref Mset.empty
let frozen_moves = ref Mset.empty
let worklist_moves = ref Mset.empty
let active_moves = ref Mset.empty

let adj_set = Hashtbl.create 17
let adj_list = ref Rmap.empty
let degree = ref Rmap.empty
let infty_deg = ref max_int

let move_list = ref Rmap.empty
let alias = ref Rmap.empty
let color = ref Rmap.empty 

let init_irc () =
  precolored := Rset.empty;
  prespilled := Rset.empty;
  initial := Rset.empty;
  simplify_worklist := Rset.empty;
  freeze_worklist := Rset.empty;
  spill_worklist := Rset.empty;
  spilled_nodes := Rset.empty;
  coalesced_nodes := Rset.empty;
  colored_nodes := Rset.empty;
  select_stack := [];
  coalesced_moves := Mset.empty;
  constrained_moves := Mset.empty;
  frozen_moves := Mset.empty;
  worklist_moves := Mset.empty;
  active_moves := Mset.empty;
  Hashtbl.clear adj_set;
  adj_list := Rmap.empty;
  degree := Rmap.empty;
  move_list := Rmap.empty;
  alias := Rmap.empty;
  color := Rmap.empty

let get_degree reg =
  try
    Rmap.find reg !degree
  with Not_found -> 0

let set_precolored u =
  initial := Rset.remove u !initial;
  if is_physical u then
    begin
      color := Rmap.add u u !color;
      precolored := Rset.add u !precolored
  end
  else
    prespilled := Rset.add u !prespilled

let add_edge u v =
  if (not (Hashtbl.mem adj_set (u,v)) && u <> v) then
    begin
      Hashtbl.add adj_set (u,v) () ;
      Hashtbl.add adj_set (v,u) () ;
      if not (Rset.mem u !precolored || Rset.mem u !prespilled) then
        begin
          adj_list := Rmap.add u 
                      (Rset.add v (find_or_empty u !adj_list))
                      !adj_list;
          degree := Rmap.add u 
                    (get_degree u + 1) 
                    !degree
      end;
      if not (Rset.mem v !precolored || Rset.mem u !prespilled) then
        begin
          adj_list := Rmap.add v 
                      (Rset.add u (find_or_empty v !adj_list))
                      !adj_list;
          degree := Rmap.add v (get_degree v + 1) !degree
      end
  end

let build graph liveness =
  let handle_instr label instr =
    let live = ref Rset.empty in
    let (use_l,def_l) = Kildall.use_def instr in
    let use_s = Register.from_list use_l in
    let def_s = Register.from_list def_l in
    Rset.iter
      (fun u -> if is_physical u then
        set_precolored u
      else if not (Rset.mem u !prespilled) then
        initial := Rset.add u !initial)
      (Rset.union use_s def_s);
    live := snd (Kildall.Lmap.find label liveness);
    (match instr with
    | Ertl.Emove(from_reg,to_reg,_) ->
      live := Rset.diff !live use_s;
      Rset.iter
        (fun r -> move_list := Rmap.add r
          (Mset.add (from_reg,to_reg) (find_or_empty_mset r
            !move_list)) !move_list)
        (Rset.union use_s def_s);
      worklist_moves := Mset.add (from_reg,to_reg) !worklist_moves
    | Ertl.EAddress(_,reg,_) ->
      set_precolored reg
    | _ -> ());
    live := Rset.union !live def_s;
    Rset.iter
      (fun d ->
        Rset.iter
          (fun l -> add_edge l d)
          !live)
      def_s
  in
  Ertl.M.iter handle_instr graph
            
let adjacent reg =
  Rset.diff
    (find_or_empty reg !adj_list)
    (Rset.union !coalesced_nodes (Register.from_list !select_stack))

let node_moves reg =
  Mset.inter (find_or_empty_mset reg !move_list) (Mset.union !active_moves
    !worklist_moves)

let move_related reg =
  not (Mset.is_empty (node_moves reg))

let mk_worklist () =
  Rset.iter
    (fun n ->
      initial := Rset.remove n !initial;
      if get_degree n >= max_deg then
        spill_worklist := Rset.add n !spill_worklist
      else if move_related n then
        freeze_worklist := Rset.add n !freeze_worklist
      else
        simplify_worklist := Rset.add n !simplify_worklist
  )
    !initial

let rec simplify () =
  let n = Rset.choose !simplify_worklist in
  simplify_worklist := Rset.remove n !simplify_worklist;
  select_stack := n::!select_stack;
  Rset.iter decrement_degree (adjacent n)

and decrement_degree m =
  let d = get_degree m in
  degree := Rmap.add m (d-1) !degree;
  if d = max_deg then
    enable_moves (Rset.add m (adjacent m));
  if (Rset.mem m !spill_worklist) && d = max_deg then
    begin
      spill_worklist := Rset.remove m !spill_worklist;
      if move_related m then
        freeze_worklist := Rset.add m !freeze_worklist
      else
        simplify_worklist := Rset.add m !simplify_worklist
  end

and enable_moves nodes =
  Rset.iter (* forall nodes *)
    (fun n ->
      Mset.iter (* forall node_moves n *)
        (fun m ->
          if Mset.mem m !active_moves then
            begin
              active_moves := Mset.remove m !active_moves;
              worklist_moves := Mset.remove m !worklist_moves
          end
      )
        (node_moves n)
  )
    nodes

let rec get_alias n =
  if Rset.mem n !coalesced_nodes then
    get_alias (Rmap.find n !alias)
  else n

let significant_degree nodes =
  let k = ref 0 in
  Rset.iter
    (fun n ->
      if get_degree n >= max_deg then
        incr k)
    nodes;
  !k

let conservative nodes =
  let k = significant_degree nodes in
  (k < max_deg)

let ok t r =
  (get_degree t < max_deg)
  || (Rset.mem t !precolored)
  || (Hashtbl.mem adj_set (t,r))

let add_work_list u =
  if (not (Rset.mem u !precolored || Rset.mem u !prespilled))
     && (not (move_related u))
     && (get_degree u < max_deg) then
    begin
      freeze_worklist := Rset.remove u !freeze_worklist;
      simplify_worklist := Rset.add u !simplify_worklist
  end

let combine u v =
  if (Rset.mem v !freeze_worklist) then
    freeze_worklist := Rset.remove v !freeze_worklist
  else
    spill_worklist := Rset.remove v !spill_worklist;
  coalesced_nodes := Rset.add v !coalesced_nodes;
  alias := Rmap.add v u !alias;
  move_list := Rmap.add u (Mset.union
    (find_or_empty_mset u !move_list)
    (find_or_empty_mset v !move_list)) !move_list;
  Rset.iter
    (fun t ->
      add_edge t u;
      decrement_degree t)
    (adjacent v);
  if (get_degree u >= max_deg) && (Rset.mem u !freeze_worklist) then
    begin
      freeze_worklist := Rset.remove u !freeze_worklist;
      spill_worklist := Rset.add u !spill_worklist
  end


let spill_cost reg =
  let (inside,outside) =
    try
      Kildall.Rmap.find reg !uses_statistics
    with Not_found -> (0,0) in 
  (float_of_int (inside + 10*outside)) /. (float_of_int (get_degree reg))

exception Not_cs_found of register * register

let select_best_move () =
  let cs = from_list callee_saved in
  try
    Mset.iter
      (fun (i,j) ->
        if (not (Rset.mem i cs) && not (Rset.mem j cs)) then
          raise (Not_cs_found(i,j)))
     !worklist_moves;
    (false,Mset.choose !worklist_moves)
  with (Not_cs_found(i,j)) -> (true,(i,j))

let exists_notcs_move () =
    fst (select_best_move ())

let coalesce () =
  let (x,y) = snd (select_best_move ()) in
  let x2 = get_alias x in
  let y2 = get_alias y in
  let (u,v) =
    if Rset.mem y !precolored || Rset.mem y !prespilled then
      (y2,x2)
    else (x2,y2) in
  worklist_moves := Mset.remove (x,y) !worklist_moves;
  if u = v then
    begin
      coalesced_moves := Mset.remove (x,y) !coalesced_moves;
      add_work_list u
  end
  else if Rset.mem v !precolored || Rset.mem v !prespilled 
          || (Hashtbl.mem adj_set (u,v)) then
    begin
      constrained_moves := Mset.add (x,y) !constrained_moves;
      add_work_list u;
      add_work_list v
  end
  else if ((Rset.mem v !precolored || Rset.mem v !prespilled)
           && (Rset.for_all (fun t -> ok t u) (adjacent v)))
          || ((not (Rset.mem v !precolored || Rset.mem v !prespilled))
              && (conservative (Rset.union (adjacent u) (adjacent v)))) then
    begin
      coalesced_moves := Mset.add (x,y) !coalesced_moves;
      combine u v;
      add_work_list u
  end
  else
      active_moves := Mset.add (x,y) !active_moves

let freeze_moves u =
  let handle u v = 
      if (Mset.mem (u,v) !active_moves) then
        active_moves := Mset.remove (u,v) !active_moves
      else
        worklist_moves := Mset.remove (u,v) !worklist_moves;
      frozen_moves := Mset.add (u,v) !frozen_moves;
      if (Mset.is_empty (node_moves v) && (get_degree v < max_deg)) then
        begin
          freeze_worklist := Rset.remove v !freeze_worklist;
          simplify_worklist := Rset.add v !simplify_worklist
      end
  in
  Mset.iter
    (fun (u,v) -> handle u v; handle v u)
    (node_moves u)

let select_best_spill () =
  let (min_cost,best_reg) =
    Rset.fold
      (fun reg (min_cost,best_reg) ->
        let curr_cost = spill_cost reg in
        if curr_cost < min_cost || min_cost < 0. then
          (curr_cost,reg)
        else (min_cost,best_reg))
      !spill_worklist (-1.,V0) in
  best_reg

let select_spill () =
  let m = select_best_spill () in
  spill_worklist := Rset.remove m !spill_worklist;
  simplify_worklist := Rset.add m !simplify_worklist;
  freeze_moves m

let freeze () =
  let u = Rset.choose !freeze_worklist in
  freeze_worklist := Rset.remove u !freeze_worklist;
  simplify_worklist := Rset.add u !simplify_worklist;
  freeze_moves u

let assign_colors () =
  while !select_stack <> [] do
    let n = List.hd !select_stack in
    select_stack := List.tl !select_stack;
    let ok_colors = ref available_colors in
    Rset.iter
      (fun w ->
        if (Rset.mem (get_alias w) (Rset.union !colored_nodes !precolored))
        then
            ok_colors := Rset.remove (Rmap.find (get_alias w) !color)
              !ok_colors
    )
      (find_or_empty n !adj_list);
    if Rset.is_empty !ok_colors then
      spilled_nodes := Rset.add n !spilled_nodes
    else
      begin
        colored_nodes := Rset.add n !colored_nodes;
        let c = Rset.choose !ok_colors in
        color := Rmap.add n c !color
    end
  done;
  Rset.iter
    (fun n ->
      try
        color := Rmap.add n (Rmap.find (get_alias n) !color) !color;
      with Not_found -> ()
  )
    !coalesced_nodes

let print_reg f = function
  | Register.Pseudo n -> Format.fprintf f "%d" n
  | r -> Print_rtl.p_pseudoreg f r

let print_graph name =
  Format.printf "graph interf {\n";
  Hashtbl.iter (fun (a,b) _ ->
    if compare a b > 0 then
      Format.printf "%a -- %a;\n" print_reg a
        print_reg b) adj_set;
  Mset.iter (fun (a,b) ->
    if compare a b > 0 then
      Format.printf "%a -- %a [style=dotted];\n" print_reg a
      print_reg b) !worklist_moves;
  Format.printf "}\n" 

let generate_coloring () =
  let coloring = ref Rmap.empty in
  let nb_spilled = ref 0 in
  Rset.iter
    (fun n ->
      coloring := Rmap.add n (Stack !nb_spilled) !coloring;
      incr nb_spilled)
    (Rset.union !prespilled !spilled_nodes);
  Rmap.iter
    (fun n c ->
      coloring := Rmap.add n (Reg c) !coloring)
    !color;
  let used_cs = Rset.inter (Register.from_list caller_saved)
                (map_image !color) in
  { nb_spilled = Rset.cardinal (Rset.union !prespilled !spilled_nodes);
    colors = !coloring;
    used_cs = Rset.elements used_cs }

let print_color f = function
  | Reg r -> Print_rtl.p_pseudoreg f r
  | Stack n -> Format.fprintf f "stack(%d)" n

let print_coloring f cl =
  Rmap.iter (fun r c -> Format.fprintf f "%a : %a\n"
    Print_rtl.p_pseudoreg r
    print_color c) cl.colors

let allocate_registers graph liveness statistics =
  uses_statistics := statistics;
  init_irc ();
  build graph liveness;
  if !print_graph_dot then
    print_graph ();
  mk_worklist ();
  while not ((Rset.is_empty !simplify_worklist) &&
  (Mset.is_empty !worklist_moves) &&
  (Rset.is_empty !freeze_worklist) &&
  (Rset.is_empty !spill_worklist)) do
    if not (Rset.is_empty !simplify_worklist) then
      simplify ()
    else if not (Mset.is_empty !worklist_moves) &&
        (exists_notcs_move () || Rset.is_empty !spill_worklist) then
      coalesce ()
    else if not (Rset.is_empty !freeze_worklist) then
      freeze ()
    else if not (Rset.is_empty !spill_worklist) then
      select_spill ()
  done;
  assign_colors ();
  let cl = generate_coloring () in
  cl

let get_color clr reg =
  if is_physical reg then Reg(reg)
  else
    begin
      let alias =
        try get_alias reg
        with Not_found -> (Format.printf "alias not found for %a.\n" 
                          Print_rtl.p_pseudoreg reg;
          assert false) in
      try
        Rmap.find alias (clr.colors)
      with Not_found -> (Format.printf 
                        "color for %a (alias is %a) not found.\n"
        Print_rtl.p_pseudoreg reg Print_rtl.p_pseudoreg alias; assert false)
  end

let spilled_count cl = cl.nb_spilled

let get_used_cs cl = cl.used_cs

