open Types

type alignement = { sizeof : int ; is_aligned : bool ; paddings : int Env.t }

let struct_alignements = ref Env.empty
let union_sizes = ref Env.empty

let round current_size =
  if current_size mod 4 <> 0 then
    current_size + 4 - (current_size mod 4)
  else current_size

let is_aligned = function
  | ET_int
  | ET_null
  | ET_star _ -> true
  | ET_struct s ->
    (Env.find s !struct_alignements).is_aligned
  | ET_union s -> fst (Env.find s !union_sizes)
  | ET_void
  | ET_char -> false


let get_sizeof = function
  | ET_void -> 0
  | ET_null
  | ET_int
  | ET_star _ -> 4
  | ET_char -> 1
  | ET_struct s ->
    (Env.find s !struct_alignements).sizeof
  | ET_union s ->
    snd (Env.find s !union_sizes)

let declare_struct name field_list =
  let aligned_found = ref false in
  let add_field (current_size,alignements) (typ,id) =
    let size = get_sizeof typ in
    let padding =
      if is_aligned typ then
        aligned_found := true;
      if is_aligned typ && current_size mod 4 <> 0 then
        round current_size
      else current_size
    in
    (padding+size,Env.add id padding alignements)
  in
  let (final_padding,align) =
     List.fold_left add_field (0,Env.empty) field_list in
  let struct_size =
    if !aligned_found then round
      final_padding
    else final_padding
  in
  struct_alignements := Env.add name {sizeof=struct_size; is_aligned =
    !aligned_found; paddings = align}
    !struct_alignements


let declare_union name field_list =
  let aligned_found = ref false in
  let max_size m (t,id) =
    let so = get_sizeof t in
    if so mod 4 = 0 then
      aligned_found := true;
    max m so
  in
  let max_s = (List.fold_left max_size 0 field_list) in
  union_sizes := Env.add name
    (!aligned_found,(if !aligned_found then round max_s else max_s))
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
    let en = (Env.find name !struct_alignements).paddings in
    Env.find field_name en
  | _ -> assert false)


