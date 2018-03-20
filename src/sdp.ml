type block_type = MAT of int | DIAG of int

let block_size = function
  | MAT s -> s
  | DIAG s -> s

type 'a sparseblock =
  { 
    entries : 'a array;
    iindices : int array;
    jindices : int array;
  }

let empty_sparseblock = { entries = [||]; iindices = [||]; jindices = [||] }

let sparseblock_map f b =
  { b with entries = Array.map f b.entries }

type 'a matrix = 'a sparseblock array

let guessed_dimension block =
  let array_max a = Array.fold_left max 0 a in
  max (array_max block.iindices) (array_max block.jindices)

let guessed_shape m =
  let dims = Array.map (fun b -> string_of_int (guessed_dimension b)) m in
  String.concat " " (Array.to_list dims)

(* operations on sparseblocks *)
let sparseblock_of_matrix zero m = (* m must be square *)
  let n = Array.length m in
  let entries, iindices, jindices = ref [],  ref [], ref [] in
  for i = 0 to n - 1 do
    for j = i to n - 1 do
      if m.(i).(j) <> zero then
	begin
	  entries := m.(i).(j) :: !entries;
	  iindices := i :: !iindices;
	  jindices := j :: !jindices
	end
    done
  done;
  { entries = Array.of_list (List.rev !entries);
    iindices = Array.of_list (List.rev !iindices);
    jindices = Array.of_list (List.rev !jindices) }
    
(* the final matrix will be symetric *)
let matrix_of_sparseblock zero n b =
  let m = Array.make_matrix n n zero in
  for i = 0 to (Array.length b.entries) - 1 do
    assert (m.(b.iindices.(i)).(b.jindices.(i)) = zero);
    assert (m.(b.jindices.(i)).(b.iindices.(i)) = zero);
    m.(b.iindices.(i)).(b.jindices.(i)) <- b.entries.(i);
    m.(b.jindices.(i)).(b.iindices.(i)) <- b.entries.(i)
  done;
  m

let nonzeros zero v =
  let aux i x =
    if x <> zero then i+1 else i in
  Array.fold_left aux 0 v

let sparseblock_of_diag zero v =
  let m = Array.length v in 
  let n = nonzeros zero v in
  let iindices = Array.make n (-1)
  and entries = Array.make n (Obj.magic 0) in
  let j = ref 0 in
  for i = 0 to m - 1 do
    if v.(i) <> zero then
      begin
	entries.(!j) <- v.(i);
	iindices.(!j) <- i;
	incr j
      end
  done;
  assert (!j = n);
{ entries = entries;
  iindices = iindices;
  jindices = iindices }

type sdp_problem =
  block_type array * float matrix * float matrix array * float array


(* printing a problem in sdpa form *)
let pf = Printf.fprintf

let h = ref [||]

let print_sparseblock out id_mat id_b b =
  let treat_entries i x =
    pf out "%d %d %d %d %e\n"
      id_mat id_b (b.iindices.(i)+1) (b.jindices.(i)+1) x in (* +1 are here to make the indices begin with 1*)
  Array.iteri treat_entries b.entries

let print_sparsematrix out id_mat mat =
  Array.iteri (fun i b -> print_sparseblock out id_mat (i+1) b) mat

let print_vector out print_elem v =
  for i = 0 to (Array.length v) - 1 do
    if i > 0 then pf out " ";
    print_elem v.(i) 
  done;
  pf out "\n"

let print_type out = function
  | MAT s -> pf out "%d" s
  | DIAG s -> pf out "-%d" s

let print_prob out (ctype, c, a, b) =
  let n_constr = Array.length b
  and n_blocks = Array.length c in
  pf out "%d\n%d\n" n_constr n_blocks;
  print_vector out (print_type out) ctype;
  print_vector out (pf out "%e") b;
  print_sparsematrix out 0 c;
  Array.iteri (fun i m -> print_sparsematrix out (i+1) m) a

let write_prob file pb =
  let out = open_out file in
  print_prob out pb;
  close_out out

let read_vector inchan =
  let list = Str.split (Str.regexp " ") (input_line inchan) in
  Array.of_list (List.rev_map float_of_string list)
                
let read_blocks inchan =
  let blocks = ref []
  and matrices = ref []
  and entries = ref []
  and iindices = ref []
  and jindices = ref []
  and nblock =  ref 1
  and nmatrices = ref 1 in
  let toArray l = Array.of_list (List.rev l) in
  let next_block () =
    incr nblock;
    blocks :=
      { entries = toArray !entries;
        iindices = toArray !iindices;
        jindices = toArray !jindices }:: !blocks;
    entries := []; iindices := []; jindices := []; in
  let next_matrix () =
    next_block ();
    incr nmatrices;
    nblock := 1;
    matrices:= (toArray !blocks):: !matrices;
    blocks := [] in
  let rec treat m b i j f =
    if m > !nmatrices then
      ( next_matrix (); treat m b i j f )
    else if b > !nblock then
      ( next_block (); treat m b i j f )
    else
      begin
        entries := f :: !entries;
        iindices := (i-1) :: !iindices;
        jindices := (j-1) :: !jindices
      end in
  begin
    try
      while true do
        let l = input_line inchan in
        Scanf.sscanf (l) "%d %d %d %d %f" treat
      done;
    with End_of_file -> ()
  end;
  next_matrix ();
  toArray !matrices

let multiply_blocks int plus times b1 b2 =
  let rec aux res a b =
    if a >= Array.length b1.iindices || b >= Array.length b2.iindices
    then res
    else 
      let i1, j1 = b1.iindices.(a), b1.jindices.(a)
      and i2, j2 = b2.iindices.(b), b2.jindices.(b) in
      if (i1, j1) = (i2, j2)
      then
        let coeff = int( if i1=j1 then 1 else 2 ) in
        aux (plus res (times coeff (times b1.entries.(a) b2.entries.(b) ))) (a+1) (b+1);
      else
        if i1 < i2 || (i1=i2 && j1<j2)
        then aux res (a+1) b
        else aux res a (b+1) in
  aux (int 0) 0 0

let multiply_matrices int plus times m1 m2 =
  assert (Array.length m1 = Array.length m2);
  let res = ref (int 0) in
  for i = 0 to ( Array.length m1 ) - 1 do
    res := plus !res (multiply_blocks int plus times m1.(i) m2.(i))
  done;
  !res
   
let multiply_blocks_float = multiply_blocks float_of_int (+.) ( *.)
let multiply_matrices_float = multiply_matrices float_of_int (+.) ( *.)
      
let load_certificate filename =
  let inchan = open_in filename in
  let v = read_vector inchan in
  let matrices = read_blocks inchan in
  close_in inchan;
(*  let out = open_out "outtest" in
  print_sparsematrix out 0 matrices.(0);
  print_sparsematrix out 0 matrices.(1);
  close_out out; *)

  Print.p (Printf.sprintf "Certificate :\n");
  Print.p (Printf.sprintf "Vector of size %d\n" (Array.length v));
  Print.p (Printf.sprintf "Plus %d matrices\n" (Array.length matrices));
  for i = 0 to Array.length matrices - 1 do
    Print.p (Printf.sprintf "Shape : %s\n" (guessed_shape matrices.(i)));
  done;

  (v,matrices)
