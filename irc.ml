open Register

module M = Map.Make(struct type t=register let compare= compare end)

module Mset = Set.Make(struct type t = (register*register)
 let compare = compare end)

type color =
  | Reg of register
  | Stack of int

type coloring = color M.t 

let max_deg = List.length available_registers

let available_colors = Kildall.from_list available_registers

let find_or_empty key map =
    try
        M.find key map
    with Not_found -> Rset.empty

let find_or_empty_mset key map =
    try
        M.find key map
    with Not_found -> Mset.empty

let precolored = ref Rset.empty
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
let adj_list = ref M.empty
let degree = ref M.empty
let infty_deg = ref 1000000 (* TODO : initialize infty_deg and make accessors for
'degree' *)

let move_list = ref M.empty
let alias = ref M.empty
let color = ref M.empty 

let print_partition () =
    Format.printf "simplify : %a\n" Kildall.p_rset !simplify_worklist;
    Format.printf "freeze : %a\n" Kildall.p_rset !freeze_worklist;
    Format.printf "spill : %a\n" Kildall.p_rset !spill_worklist;
    Format.printf "initial : %a\n" Kildall.p_rset !initial;
    Format.printf "spilled : %a\n" Kildall.p_rset !spilled_nodes;
    Format.printf "coalesced : %a\n" Kildall.p_rset !coalesced_nodes;
    Format.printf "colored : %a\n" Kildall.p_rset !colored_nodes;
    Format.printf "precolored : %a\n" Kildall.p_rset !precolored;
    Format.printf "stack : %a\n" Kildall.p_rset (Kildall.from_list
    !select_stack)

let init_irc () =
    precolored := Rset.empty;
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
    adj_list := M.empty;
    degree := M.empty

let get_degree reg =
    try
        M.find reg !degree
    with Not_found -> 0

let set_precolored u =
    precolored := Rset.add u !precolored;
    color := M.add u u !color

let add_edge u v =
    if (not (Hashtbl.mem adj_set (u,v)) && u <> v) then
     begin
         Hashtbl.add adj_set (u,v) () ;
         Hashtbl.add adj_set (v,u) () ;
         if not (Rset.mem u !precolored) then
          begin
              adj_list := M.add u (Rset.add v (find_or_empty u !adj_list)) !adj_list;
              degree := M.add u (get_degree u + 1) !degree
          end;
         if not (Rset.mem v !precolored) then
          begin
              adj_list := M.add v (Rset.add u (find_or_empty v !adj_list)) !adj_list;
              degree := M.add v (get_degree v + 1) !degree
          end
     end

let build graph liveness =
    let handle_instr label instr =
        let live = ref Rset.empty in
        let (use_l,def_l) = Kildall.use_def instr in
        let use_s = Kildall.from_list use_l in
        let def_s = Kildall.from_list def_l in
        Rset.iter
        (fun u -> if is_physical u then
            set_precolored u
        else initial := Rset.add u !initial)
        (Rset.union use_s def_s);
        live := snd (Kildall.Lmap.find label liveness);
        (match instr with
         | Ertl.Emove(from_reg,to_reg,_) ->
                 live := Rset.diff !live use_s;
                 Rset.iter
                 (fun r -> move_list := M.add r
                    (Mset.add (from_reg,to_reg) (find_or_empty_mset r
                    !move_list)) !move_list)
                 (Rset.union use_s def_s);
                 worklist_moves := Mset.add (from_reg,to_reg) !worklist_moves
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
      (Rset.union !coalesced_nodes (Kildall.from_list !select_stack))

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
    degree := M.add m (d-1) !degree;
    if d = max_deg then
        enable_moves (Rset.add m (adjacent m));
        if (Rset.mem m !spill_worklist) then
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
        get_alias (M.find n !alias)
    else n

let conservative nodes =
    let k = ref 0 in
    Rset.iter
    (fun n ->
        if get_degree n >= max_deg then
            incr k)
    nodes;
    (!k < max_deg)

let ok t r =
    (get_degree t < max_deg)
         || (Rset.mem t !precolored)
         || (Hashtbl.mem adj_set (t,r))

let add_work_list u =
    if (not (Rset.mem u !precolored))
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
    alias := M.add v u !alias;
    move_list := M.add u (Mset.union
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

let coalesce () =
    let (x,y) = Mset.choose !worklist_moves in
    let x2 = get_alias x in
    let y2 = get_alias y in
    let (u,v) =
        if Rset.mem y !precolored then
            (y2,x2)
        else (x2,y2) in
    worklist_moves := Mset.remove (x,y) !worklist_moves;
    if u = v then
      begin
        coalesced_moves := Mset.remove (x,y) !coalesced_moves;
        add_work_list u
      end
    else if Rset.mem v !precolored || (Hashtbl.mem adj_set (u,v)) then
      begin
        constrained_moves := Mset.add (x,y) !constrained_moves;
        add_work_list u;
        add_work_list v
      end
    else if (Rset.mem v !precolored
        && (Rset.for_all (fun t -> ok t u) (adjacent v)))
        || ((not (Rset.mem v !precolored)) && (conservative (Rset.union
        (adjacent u) (adjacent v)))) then
      begin
          coalesced_moves := Mset.add (x,y) !coalesced_moves;
          combine u v;
          add_work_list u
      end
    else
      begin
        active_moves := Mset.add (x,y) !active_moves
      end

let freeze_moves u =
    Mset.iter
    (fun (u,v) -> (* TODO : (v,u) *)
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
    )
    (node_moves u)

let select_spill () =
    let m = Rset.choose !spill_worklist in (* TODO : smarter choice ? *)
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
                ok_colors := Rset.remove (M.find (get_alias w) !color)
                !ok_colors
        )
        (find_or_empty n !adj_list);
        if Rset.is_empty !ok_colors then
            spilled_nodes := Rset.add n !spilled_nodes
        else
          begin
            colored_nodes := Rset.add n !colored_nodes;
            let c = Rset.choose !ok_colors in
            color := M.add n c !color
          end
    done;
    Rset.iter
    (fun n ->
        try
            color := M.add n (M.find (get_alias n) !color) !color;
        with Not_found -> ()
    )
    !coalesced_nodes

let print_reg f = function
   | Register.Pseudo n -> Format.fprintf f "%d" n
   | r -> Print_rtl.p_pseudoreg f r

(* This function needs Ocaml 3.12 *)

   (*
let print_graph () =
    Format.eprintf "graph testg {\n";
    let node_count = ref 0 in
    M.iter
    (fun m _ ->
        let theta = (2.*.3.14159*.(float_of_int !node_count) /.
        (float_of_int (M.cardinal !adj_list))) in
        Format.eprintf "%a [pos=\"%d,%d!\"];\n" Print_rtl.p_pseudoreg m
        (int_of_float (100.*.cos theta)) (int_of_float (100.*.sin theta));
        incr node_count)
    !adj_list;
    Hashtbl.iter (fun (a,b) _ ->
        if compare a b > 0 then
            Format.eprintf "%a -- %a;\n" print_reg a
             print_reg b) adj_set;
    Format.eprintf "}\n" 
*)

let generate_coloring () =
    let coloring = ref M.empty in
    let nb_spilled = ref 0 in
    Rset.iter
    (fun n ->
        coloring := M.add n (Stack !nb_spilled) !coloring;
        incr nb_spilled)
    !spilled_nodes;
    M.iter
    (fun n c ->
        coloring := M.add n (Reg c) !coloring)
    !color;
    !coloring

let print_color f = function
   | Reg r -> Print_rtl.p_pseudoreg f r
   | Stack n -> Format.fprintf f "stack(%d)" n

let print_coloring f =
    M.iter (fun r c -> Format.fprintf f "%a : %a\n"
            Print_rtl.p_pseudoreg r
            print_color c)

let allocate_registers graph liveness =
    init_irc ();
    build graph liveness;
    mk_worklist ();
    while not ((Rset.is_empty !simplify_worklist) &&
               (Mset.is_empty !worklist_moves) &&
               (Rset.is_empty !freeze_worklist) &&
               (Rset.is_empty !spill_worklist)) do
         if not (Rset.is_empty !simplify_worklist) then
             simplify ()
         else if not (Mset.is_empty !worklist_moves) then
             coalesce ()
         else if not (Rset.is_empty !freeze_worklist) then
             freeze ()
         else if not (Rset.is_empty !spill_worklist) then
            select_spill ()
    done;
    assign_colors ();

   (* print_graph (); *)
    let cl = generate_coloring () in
    print_coloring Format.std_formatter cl; cl

let get_color cl reg =
    M.find reg cl


