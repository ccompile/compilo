
let string_counter = ref 1
let currently_aligned = ref true

let data_segment = ref []

let set_alignement align =
  if align != !currently_aligned then
    begin
      data_segment := (Mips.Align (if align then 2 else 0))
        ::(!data_segment);
      currently_aligned := align
  end

let declare_global id typ =
  let name = Printf.sprintf "g_%s"
    id in
  let sizeof = Sizeof.get_sizeof typ in
  set_alignement (Sizeof.is_aligned typ);
  data_segment := (Mips.Space (name,sizeof))::(!data_segment)

let get_global_label id =
  Printf.sprintf "g_%s" id

let declare_string value =
  let name = Printf.sprintf "str_%d"
    (!string_counter) in
  incr string_counter;
  set_alignement false;
  data_segment := (Mips.Asciiz (name,value))::(!data_segment);
  name

let get_data () =
  List.rev !data_segment

