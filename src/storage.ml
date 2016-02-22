open Common

(* reading/writing files *)
let dir = "save/"

(*let _ = if not (Sys.file_exists dir) then Unix.mkdir dir 0o644*)

let save_marshal filename obj =
  let name = Printf.sprintf "%s%s" dir filename in
  let outChannel = open_out name in
  Marshal.to_channel outChannel obj [];
  close_out outChannel

let load_marshal filename =
  let name = Printf.sprintf "%s%s" dir filename in
  if Sys.file_exists name then
      let inChannel = open_in name in
      let obj = Marshal.from_channel inChannel in
      close_in inChannel;
      Print.debug " Found.\n";
      obj
  else
    raise Not_found

(* general function *)
let load_or_create create filename =
  Print.debug (Printf.sprintf "Looking for %s..." filename);
  try
    load_marshal filename
  with
  | Not_found ->
    Print.verbose (Printf.sprintf "\nComputing file %s\n" filename);
    let obj = create () in
    Print.verbose (Printf.sprintf "Writing in file %s... " filename);
    save_marshal filename obj;
    Print.verbose " Done\n";
    flush stdout;
    obj

(*let table = Hashtbl.create 10

let load_or_create create filename =
  try 
    Obj.magic (Hashtbl.find table filename)
  with
  | Not_found ->
    let x = load_or_create create filename in
    Hashtbl.add table filename (Obj.magic x);
    x*)

(* basis id's *)
type 'a basis_id =
  {
    flagType : string;
    flagSize : int;
    typeSize : int;
    typeId : int
  }

let mul_basis b1 b2 =
  assert (b1.flagType = b2.flagType);
  assert (b1.typeSize = b2.typeSize);
  assert (b1.typeId = b2.typeId);
  { b1 with flagSize = b1.flagSize + b2.flagSize - b1.typeSize }

let untype_basis b =
  { b with typeSize = 0; typeId = 0 }

let square_basis b =
  mul_basis b b

let div_basis b1 b2 =
  assert (b1.flagType = b2.flagType);
  assert (b1.typeSize = b2.typeSize);
  assert (b1.typeId = b2.typeId);
  let new_size = b1.flagSize - b2.flagSize + b1.typeSize in
  assert (new_size >= b1.typeSize);
  { b1 with flagSize = new_size }

(* file names *)
let flag_dir id =
  id.flagType

let basis_name id =
  Printf.sprintf "%s/basis%d_type%d_id%d"
    (flag_dir id) id.flagSize id.typeSize id.typeId

let p_name id1 id2 =
  Printf.sprintf "%s/p_%din%d_type%d_id%d"
    (flag_dir id1) id1.flagSize id2.flagSize id1.typeSize id1.typeId

let p2_name id1 id2 id3 =
  Printf.sprintf "%s/p2_%d_%din%d_type%d_id%d"
    (flag_dir id1) id1.flagSize id2.flagSize id3.flagSize id1.typeSize id1.typeId

let q_name id =
  Printf.sprintf "%s/q_%d_type%d_id%d"
    (flag_dir id) id.flagSize id.typeSize id.typeId

let untype_name id =
  Printf.sprintf "%s/untype_%d_type%d_id%d"
    (flag_dir id) id.flagSize id.typeSize id.typeId

(* tabulating and storing *)
module Make ( Flag : Flag.S ) =
struct

  module A = Algebra.Make (Flag)
  open A

  (* single computations *)
  let get_p_denom id1 id2 =
    p_denom id1.typeSize id1.flagSize id2.flagSize

  let get_p2_denom id1 _ id3 =
    p2_denom id1.typeSize id1.flagSize id3.flagSize
      
  let get_q_denom id =
    q_denom id.typeSize id.flagSize
  
  (* specific basis *)
  let basis_id size typeSize typeId =
    {
      flagType = A.flagname;
      flagSize = size;
      typeSize = typeSize;
      typeId = typeId
    }

  let untyped_basis_id size =
    basis_id size 0 0

  let check b =
    b.flagType = Flag.name

  let same_type b1 b2 =
    ( b1.typeSize = b2.typeSize ) && ( b1.typeId = b2.typeId )
    
  (* tabulating recipes *)

  let rec make_basis id =
    match id.flagSize, id.typeSize with
    | 1, 0 -> span_flags 1
    | n, 0 ->
      let b = Array.to_list(get_basis (untyped_basis_id (n - 1))) in
      span_flags_next b
    | n, k ->
      let bk = get_basis (untyped_basis_id k)
      and bn = Array.to_list(get_basis (untyped_basis_id n)) in
      span_all_typed bk.(id.typeId) bn

  and get_basis id =
    assert (check id);
    load_or_create (fun () -> Array.of_list (make_basis id)) (basis_name id)
      
  let rec make_p id1 id2 =
    match id2.flagSize - id1.flagSize with
    | 0 | 1 ->
      let k = id1.typeSize
      and b1 = get_basis id1
      and b2 = get_basis id2 in
      p_tabulate k b1 b2
    (* Array.map (fun h -> Array.map (p k h) b2) b1 *) (* TO BE REMOVED *)
    | k when k > 1 -> 
      let id3 = basis_id (id1.flagSize+1) id1.typeSize id1.typeId in
      let p13 = get_p id1 id3
      and p32 = get_p id3 id2 in
      let scale =
	((get_p_denom id1 id3)*(get_p_denom id3 id2))/(get_p_denom id1 id2) in
      let res = multiply_matrix 0 ( + ) ( * ) p32 p13 in
      matrix_map (fun x -> x / scale) res
    | _ -> failwith "make_p : bad arguments"    

  and get_p id1 id2 =
    assert ( check id1 && check id2 );
    assert ( same_type id1 id2 );
    load_or_create (fun () -> make_p id1 id2) (p_name id1 id2)

  let make_p2 id1 id2 id3 =
    let k = id1.typeSize
    and b1 = get_basis id1
    and b2 = get_basis id2
    and b3 = get_basis id3 in
    p2_tabulate k b1 b2 b3

  let get_p2 id1 id2 id3 = (* rq : id3 redondant *)
    assert ( check id1 && check id2 && check id3 );
    assert ( same_type id1 id3 );
    assert ( same_type id2 id3 );
    assert ( id3 = mul_basis id1 id2 );
    load_or_create (fun () -> make_p2 id1 id2 id3) (p2_name id1 id2 id3)
      
  let make_q id =
    let k = id.typeSize
    and b = get_basis id in
    Array.map (q_nom k) b
      
  let get_q id =
    assert (check id);
    load_or_create (fun () -> make_q id) (q_name id)
      
  let make_untype id =
    let id0 = untyped_basis_id id.flagSize in
    let b = get_basis id
    and b0 = get_basis id0 in
    let untype h = Common.array_search b0 (normal_form h) in 
    Array.map untype b
  
  let get_untype id =
    assert (check id);
    load_or_create (fun () -> make_untype id) (untype_name id)
      
  (* not stored *)
  let sizeTbl = Hashtbl.create 10
  let _ = Hashtbl.add sizeTbl (untyped_basis_id 0) 1

  let get_size id =
    assert (check id);
    try
      Hashtbl.find sizeTbl id
    with
    | Not_found ->
      let l = Array.length (get_basis id) in
      Hashtbl.add sizeTbl id l;
      l

  let id_flag id flag =
    assert (check id);
    assert (id.flagSize = Flag.size flag);
    array_search (get_basis id) (A.normal_form_typed id.typeSize flag);
end
