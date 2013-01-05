open Types

type alignement = (int (* sizeof *) * int Env.t (* paddings *))

let struct_alignements = ref Env.empty
let union_sizes = ref Env.empty

let round current_size =
   if current_size mod 4 <> 0 then
       current_size + 4 - (current_size mod 4)
   else current_size

let get_sizeof = function
    | ET_void -> 0
    | ET_null
    | ET_int
    | ET_star _ -> 4
    | ET_char -> 1
    | ET_struct s ->
        fst (Env.find s !struct_alignements)
    | ET_union s ->
        Env.find s !union_sizes

let declare_struct name field_list =
    let add_field (current_size,alignements) (typ,id) =
        let size = get_sizeof typ in
        let padding =
            if size mod 4 = 0 && current_size mod 4 <> 0 then
                round current_size
            else current_size
        in
        (padding+size,Env.add id padding alignements)
    in
    let (final_padding,align) = List.fold_left add_field (0,Env.empty) field_list in
    struct_alignements := Env.add name (round final_padding,align)
        !struct_alignements


let declare_union name field_list =
    let max_size m (t,id) =
        max m (get_sizeof t)
    in
    union_sizes := Env.add name
    (round (List.fold_left max_size 0 field_list))
    !union_sizes

let declare_type name = function
    | UnionSig field_list ->
        declare_union name field_list
    | StructSig field_list ->
        declare_struct name field_list

let get_offset typ field_name =
    (match typ with
    | ET_union _ -> 0
    | ET_struct name ->
            let (_,en) = Env.find name !struct_alignements in
            Env.find field_name en
    | _ -> assert false)


