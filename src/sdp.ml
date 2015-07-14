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

(* drawing functions *)
(*let draw_sparseblock print_elem x0 y0 c b =
  let n = Array.length b.entries in
  for i = 0 to n-1 do
    Graphics.moveto (x0 + b.iindices.(i)*c) (y0 - b.jindices.(i)*c);
    Graphics.draw_string (print_elem b.entries.(i))
  done
*)
(*
let draw_matrix print_elem sizes x0 y0 c m =
  let n = Array.length m in
  let total = sizes.(n) in
  Graphics.moveto (x0-c/2) (y0 + c/2);
  Graphics.lineto (x0-c/2) (y0 - total*c);
  Graphics.moveto (x0+total*c+c/2) (y0 + c/2);
  Graphics.lineto (x0+total*c+c/2) (y0 - total*c);
  for i = 0 to n-1 do
    draw_sparseblock print_elem (x0+sizes.(i)*c) (y0-sizes.(i)*c) c m.(i)
  done

 let draw_prob c ctype a b =
  let step = 30 in
  let n = Array.length ctype in
  let sizes = Array.make (n+1) 0 in
  for i = 0 to n-1 do
    sizes.(i+1) <- sizes.(i) + (block_size ctype.(i))
  done;
  let total = sizes.(n) + 3 in
  let m = Array.length a in
  let hmax = (2*total+2)*step in
  let draw i j m =
    draw_matrix (Printf.sprintf "%.1f") sizes
      (10 + i*step)
      (hmax - 10 - j*step) step m in
  Graphics.open_graph (Printf.sprintf " 1200x%d" hmax);
  for i = 0 to m-1 do
    draw 0 i  a.(i)
  done;
  draw 1 0 c;
  let _ = Graphics.read_key () in
  Graphics.close_graph ()
*)  



(* parsing *)
(*
let stream_of_file file =
  let inchan = open_in file in
  let next _ =
    try Some (input_line inchan)
    with End_of_file -> close_in inchan; None in
  Stream.from next

let read_block_line =...*)

(* tests *)

(*
let c = [| sparseblock_of_matrix [|[|2.;1.|];
				   [|1.;2.|]|];
	   sparseblock_of_matrix [|[|3.;0.;1.|];
				   [|0.;2.;0.|];
				   [|1.;0.;3.|]|];
	   empty_sparseblock|]
  
let a1 = [|sparseblock_of_matrix [|[|3.;1.|];
				   [|1.;3.|]|];
	   empty_sparseblock;
	   sparseblock_of_matrix [|[|1.;0.|];
				   [|0.;0.|]|]|]
  
let a2 = [|empty_sparseblock;
	   sparseblock_of_matrix [|[|3.;0.;1.|];
				   [|0.;4.;0.|];
				   [|1.;0.;5.|]|];
	   sparseblock_of_matrix [|[|0.;0.|];
				   [|0.;1.|]|]|]
  
let b = [|1.;2.|]

let _ = write_prob "prob" c [|MAT 2; MAT 3; DIAG 2|] [|a1;a2|] b
*)
