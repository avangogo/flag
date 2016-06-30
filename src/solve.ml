(** Main generic function for building sdpa instances *)

open Sdp
open Storage
open Common
open Vectors
open Inequality

type 'a constraintblock =
  { block_type : block_type;
    bflags : 'a sparseblock array;
    bbound : 'a sparseblock }

(* operation on constraintblocks *)
let constraintblock_map f b =
  {
    block_type = b.block_type;
    bflags = Array.map (sparseblock_map f) b.bflags;
    bbound = sparseblock_map f b.bbound
  }

(* print functions *)
let sprintf = Printf.sprintf

let print_ineq_infos_0 p_scal ineq =
  let assoc = makeAssoc (List.map (fun i -> i.name, i) ineq) in
  let treat (name, l) =
    Print.p ~color:Print.yellow (sprintf "%s (%d)\n"
	       (option_default "No label" name) (List.length l));
    let l' = List.map (Inequality.print p_scal) l in
    let l'' = makeAssoc (List.map (fun i -> (i,())) l') in
    List.iter (fun (s, l) ->
      Print.verbose (sprintf "%s (%d)\n" s (List.length l))) l'' in
  List.iter treat assoc


let print_ineq_infos p_scal ineq =
  Print.p (sprintf "Building Inequalities block (%d inequalities)\n"
	     (List.length ineq));
  Print.p ~color:Print.yellow "Inequalities list:\n";
  print_ineq_infos_0 p_scal ineq

let time = ref 0.

let tic () =
  time := Sys.time ()

let tac s =
  Print.p ~color:Print.red (Printf.sprintf "%s time : %.3f\n" s (Sys.time () -. !time))

    
(* optimized bracket operator *)
let int_untype n q untype v =
  let res = Array.make n 0 in
  for i = 0 to (Array.length v) - 1 do
    res.(untype.(i)) <- res.(untype.(i)) + q.(i)*v.(i)
  done;
  res
   
(* much can be done if efficiency is required here *)
(* (sparse structure, doing everything in one step, *)
(* computing just half of the coefficients of the symetric matrix..) *)

    
module Make ( Flag : Flag.S ) ( F : Field.S ) =
struct
  module S = Storage.Make ( Flag )
  open S


(*  let opt_untype b untype d_q int_q d v =
    let n = get_size (untype_basis b) in
    let int_v = rat_to_int_array d v in
    let int_res = int_untype n int_q untype int_v in
    let d_res = d_q * d in
    Array.map (fun i -> Rational.make i d_res) int_res *)

  let opt_untype b untype q v =
    let n = get_size (untype_basis b) in
    let res = int_untype n q untype v in
    Array.map Rational.int res     

      
  (* bracketed multiplication table *)
(*  let mul_table b =
    (* the basis where products live *)
    let b2 = square_basis b in
    (* *)
    let p2 = get_p2 b b b2 in
    (* precomputed data *)
(*    let d =  denom_p2_square b in
      let d_q = denom_q b2 in
      let int_q = rat_to_int_array d_q (get_q b2) in *)
    let q = get_q b2 in
    let untype = get_untype b2 in
    matrix_map (opt_untype b2 untype q) p2
*)  
    
  let mul_table2 b =
    (* the basis where products live *)
    let b2 = square_basis b in
    (* multiplication table *)
    let p2 = get_p2 b b b2 in
    (* unlabeloperation *)
    let q = get_q b2 in
    let untype = get_untype b2 in
    let n = get_size (untype_basis b2) in
    (* *)
    let rev_untype = Combinatoric.pre_image n untype in
    (* *)
    Array.map (fun select -> Sparse.linear_combination select q p2) rev_untype
      
  (* returns bflags=[|A0,..,Am-1|] such that Ai*X (coefficient by coefficient multiplication) is the coefficient of Fi in the Cauchy-Schwarz inequality with sdp matrix X *)
  let cs_block b =
    Print.verbose (Printf.sprintf "cs_block : %s\n" (basis_name b));
(*    let m = mul_table b in
      let inv_m = matrix_array_of_array_matrix m in *)
    let m = mul_table2 b in
    let m_sparseblock = Array.map Sparse.to_sparseblock m in
    {
      block_type = MAT (Sparse.n m.(0));
      bflags = Array.map (Sdp.sparseblock_map float_of_int) m_sparseblock;
      bbound = empty_sparseblock
    }
      
  let inequality_block zero b v =
    let m = get_size b in (* size of the basis *)
    let n = Array.length v in (* number of inequalities *)
    let flags = Array.make m empty_sparseblock in
    for i = 0 to m-1 do
      flags.(i) <- sparseblock_of_diag zero
	(Array.init n (fun j -> v.(j).flags.vect.(i)))
    done;
    let bound = sparseblock_of_diag zero
      (Array.init n (fun j -> v.(j).bound)) in
    {
      block_type = DIAG n;
      bflags = flags;
      bbound = bound
    }
      
  let all_rooted_basis n sigma =
    let b_sigma = untyped_basis_id sigma in
    let m = get_size b_sigma in
    list_init m ( fun i -> basis_id n sigma i )
      
  let all_cs b =
    assert ( b.typeSize = 0 && b.typeId = 0 );
    let res = ref [] in
    let n = b.flagSize in
    for m = (n+1)/2 to (2*n - 1)/2 do
      let sigma = 2*m - n in
      res := (all_rooted_basis m sigma) :: !res
    done;
    List.concat !res

  let make_problem b cons_array obj =
    let types = Array.map (fun c -> c.block_type) cons_array in
    let bound = Array.map (fun c -> c.bbound) cons_array in
    let n = get_size b in
    let coeff i = Array.map (fun c -> c.bflags.(i)) cons_array in
    let flags = Array.init n coeff in
    (types, bound, flags, obj)    

  let solve file cs ineq obj =

    let b = obj.basis in
    
    Print.p (Printf.sprintf "Building sdp problem (%s)\n" file);
    Print.verbose(Printf.sprintf "Flag basis : %s\n" (basis_name b));
    Print.verbose(Printf.sprintf "Size of the basis : %d\n" (S.get_size b));
    Print.p (Printf.sprintf "Maximizing : %s\n" (option_default "." obj.name));
    assert (List.for_all (fun b0 -> untype_basis (square_basis b0) = b) cs);
    assert (List.for_all (fun i -> i.flags.basis = b) ineq);

    (* Cauchy-Schwartz *)
    Print.p
      (Printf.sprintf "Building Cauchy-Schwartz blocks (%d bases)\n"
	 (List.length cs));

    let cs_blocks = Array.map cs_block (Array.of_list cs) in

    (* Inequalities *)
    print_ineq_infos F.print ineq;
    
    let all_ineq =
      (List.map (Inequality.map F.to_float) ineq) in

    let ineq_block =
      inequality_block 0. b (Array.of_list all_ineq) in

    Print.p "Merging\n";

    let cons = Array.append cs_blocks [|ineq_block|]
    and obj = Array.map F.to_float obj.vect in

    Print.p "Formating\n";    

    let pb = make_problem b cons obj in

    let sdpa_file = Printf.sprintf "%s.sdpa" file in

    Print.p (sprintf "Writing problem in file \"%s\"\n" sdpa_file);

    if !Param.latex
    then Latexify.pb_to_file (Printf.sprintf "latex/%s.tex" file) pb;
 
    write_prob sdpa_file pb
end
