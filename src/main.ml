(* Caccetta-Haggkvist *)

open Digraph
open Vectors
open Storage

module S = Storage.Make (Trianglefree)
module I = Inequality.Make (Field.Float) (Trianglefree)
module Vect = Vectors.Vect (Field.Float) (Trianglefree)
module Solve = Solve.Make (Trianglefree) (Field.Float)
open Vect (* Contains the infix operators +~, -~ and *~ on vectors *)
open I

(* Basis on which we build the sdp problem *)
let b = S.untyped_basis_id 6 (* <- max size of flags *)

(* Constant in the conjecture *)
let c0 = 0.3425

(* **** sigma_sources ***** *)

(* the function f *)
(* sum of sigma sources *)
let is_a_sigma_source sigma flag = (* sigma is the size of the type *)
  let edge_condition (i, j) =
    not (i >= sigma && j < sigma) in
  Common.array_for_all edge_condition flag.e

let sum_of_sigma_sources basis =
  let indicator sigma f =
    if (is_a_sigma_source sigma f) then 1. else 0. in
  Vect.make ~name:"sigma sources" basis indicator

(* building f0 *)
let f0_flag sigma_flag = (* sigma is a type represented by a flag *)
  let n = sigma_flag.n in
  let edges = Array.concat [sigma_flag.e; Array.init n (fun i -> (i, n))] in
  Digraph.make (n+1) edges

(* Returns the type of the basis (as a graph) *)
let get_type basis =
  let type_basis = { basis with flagSize = basis.typeSize } in
  (S.get_basis type_basis).(0)

(* If basis has a type of size k, returns the unique flag of size k+1
on this basis such that the vertex not labeled has indegree k *)
let f0 basis =
  let f0 = f0_flag (get_type basis) in
  Vect.flag ~name:"f0" basis f0
    
(* Indicator vector of the in-neighbourhood of v *)
let in_nbrhd_indicator g v =
  let res = Array.make g.n false in
  Array.iter (function (u,w) -> if w=v then res.(u) <- true) g.e;
  res



    
(*  *)
let f basis =
  let x_f0 = Vect.scalar_mul ~name:"(c0 - 1)" (c0 -. 1.) (f0 basis) in
  let c0_one = Vect.scalar_mul ~name:"c0" c0 (Vect.one basis) in
  let sum = sum_of_sigma_sources basis in
  sum +~ x_f0 -~ c0_one

let f_inequality basis =
  at_least (Vect.untype (f basis)) 0.

(* *)
let has_dominated_vertex g =
  Common.array_exists ( (==) (g.n - 1) ) (in_degrees g)

let f_inequalities =
  let res = ref [] in
  let n = b.flagSize - 1 in
  let types = S.get_basis (S.untyped_basis_id n) in
  for i = 0 to (Array.length types) - 1 do
    if has_dominated_vertex types.(i) then
      let i = f_inequality (S.basis_id (n+1) n i) in
      res := i :: !res
  done;
  !res

(* ***** new f ***** *)
    
(* Returns the list of pairs (id_g, ind),
for evey graph g and and every vertex v of g of indegree size of g - 1
where id_g is the identificator of g and ind the in neighbourhood of v *)
let types_with_pseudo_dominated_vertex size =
  let b = S.untyped_basis_id size in
  let flags = S.get_basis b in (* graphs of size [size] *)
  let res = ref [] in (* result container *)
  for i = 0 to (Array.length flags) - 1 do
    let indegree = in_degrees flags.(i)
    and outdegree = out_degrees flags.(i) in
    for v = 0 to size - 1 do
      if ( indegree.(v) >= size - 2 ) && ( outdegree.(v) = 0 ) then
	let in_ind = in_nbrhd_indicator flags.(i) v in
	res := (i, in_ind) :: !res
    done
  done;
  !res

let indicator ind sigma g =
  assert (g.n = sigma + 1);
  let edge_condition (u, v) = not ( u = sigma && ind.(v) ) in
  if Common.array_for_all edge_condition g.e then 1. else 0.
    
let new_f typesize (i, ind) =
  let n = typesize + 1 in
  let b = S.basis_id n typesize i in
  let sum = Vect.make ~name:"pseudo sigma sources" b (indicator ind) in
  let x_f0 = Vect.scalar_mul ~name:"(c0 - 1)" (c0 -. 1.) (f0 b) in
  let c0_one = Vect.scalar_mul ~name:"c0" c0 (Vect.one b) in
  sum +~ x_f0 -~ c0_one

    
let new_f_inequalities n =
  let typeSize = n - 1 in
  let types = types_with_pseudo_dominated_vertex typeSize in
  let make_inequality (i, ind) =
    at_least (new_f typeSize (i, ind)) 0. in
  List.map make_inequality types

let new_f_ineq_exp n b =
  List.concat (List.map (multiply_and_unlabel b) (new_f_inequalities n))
    
(* constructing indV and indT *)

let b3 = S.untyped_basis_id 3
let b4 = S.untyped_basis_id 4

(*
let iT = S.id_flag b3 (Digraph.make 3 [|(0,1);(1,2);(0,2)|])
let iV = S.id_flag b3 (Digraph.make 3 [|(0,2);(1,2)|])

let ind i n =
  let basis = S.basis_id (n+1) n i in
  Vect.untype (f basis)

let indV = ind iV 3
  let indT = ind iT 3*)
  
(* flags rooted on one vertex *)
let b2_1 = S.basis_id 2 1 0
let b3_1 = S.basis_id 3 1 0
let alpha = Vect.flag ~name:"alpha" b2_1 (Digraph.make 2 [|(0,1)|])
let chi = Vect.flag ~name:"chi" b3_1 (Digraph.make 3 [|(0,1);(0,2)|])
  
(* ***** forks *********** *)
 
(* Building fork *)
let kappa = Vect.flag ~name:"kappa" b3 (Digraph.make 3 [|(0,1);(0,2)|])
  
let sausage_const = 0.88 (*0.88*)
  
let fork = (* fork : kappa - (3*(3*c0-1)^2)/sausage *)
  let x = 3. *. c0 -. 1. in
  let y = (3. *. x *. x)/.sausage_const in
  kappa -~ (Vect.scalar_mul y (Vect.one b3))

(* fork inequality : chi - (1/sausage)*(c0 - alpha)^2 *)
let rooted_fork =
  let x =
    (Vect.scalar_mul c0 (Vect.one b2_1)) -~ (Vect.expand b2_1 alpha) in
  chi -~ ( Vect.scalar_mul (1. /. sausage_const) (x *~ x) )
    
(* let _ = Vect.draw rooted_fork
   let _ = Vect.draw (Vect.untype rooted_fork) *)

  
(* ************ min deg *************** *)
let b_1 = { b with typeSize = 1; typeId = 0 }

let alpha_geq_c0 =
  at_least alpha c0

(* Debugage *)
let time = ref 0.
  
let tic () =
  time := Sys.time ()

let tac s =
  Print.p ~color:Print.red (Printf.sprintf "%s time : %.3f\n" s (Sys.time () -. !time))

let _ = tic () 

let alpha_is_c0 =
  name_list "Outdegree is c0"
    (List.concat (List.map equality
		    (multiply_and_unlabel b (alpha_geq_c0))))

let _ = tac "Outdegree is c0"
    
(* ******* Construction of the problem ********* *)
let cauchy_schwartz = Solve.all_cs b

let x1 = I.name_list "new f : n" ( new_f_ineq_exp (b.flagSize) b )
let x2 = I.name_list "new f : n-1" ( new_f_ineq_exp (b.flagSize - 1) b )
let x3 = I.name_list "new f : n-2" ( new_f_ineq_exp (b.flagSize - 2) b )
let x4 = I.name_list "new f : n-3" ( new_f_ineq_exp (b.flagSize - 3) b )
  
let inequalities =
  List.concat
    [
      all_flags_nonnegative b;
      equality (totalsum b);
      [ (* expand b (at_least indV 0.); *)
	(* expand b (at_least indT 0.); *)
	expand b (at_least fork 0.) ];
      f_inequalities;
      multiply_and_unlabel b (at_least rooted_fork 0.);
      x1; x2; x3; x4;
      alpha_is_c0;
    ]

let b2 = S.untyped_basis_id 2
let edge = Vect.flag ~name:"edge" b2 (Digraph.make 2 [|(0, 1)|])
let obj = Vect.expand b (Vect.scalar_mul (-1.) edge)

let _ = Solve.solve "ch" cauchy_schwartz inequalities obj

(*  S.basis_id *)
