(* Caccetta-Haggkvist *)

open Digraph
open Vectors
open Storage
module S = Storage.Make (Trianglefree)
module I = Inequality.Make (Field.Float) (Trianglefree)
module Vect = Vectors.Vect (Field.Float) (Trianglefree)
module RatVect = Vectors.Vect (Rational) (Trianglefree)
module Solve = Solve.Make (Trianglefree)
open Solve
open I

let b = S.untyped_basis_id 6
  
let c0 = 0.348

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
  Vect.make ~name:"sum of sigma sources" basis indicator

(* building f0 *)
let f0_flag sigma = (* sigma is a type represented by a flag *)
  let n = sigma.n in
  let edges = Array.concat [sigma.e; Array.init n (fun i -> (i, n))] in
  Digraph.make (n+1) edges

(* *)
let get_type basis =
  let type_basis = { basis with flagSize = basis.typeSize } in
  (S.get_basis type_basis).(0)

let f0 basis =
  let f0 = f0_flag (get_type basis) in
  Vect.flag ~name:"f0" basis f0

(*  *)

(*  *)
let f basis =
  let x_f0 = Vect.scalar_mul ~name:"(c0 - 1)" (c0 -. 1.) (f0 basis) in
  let c0_one = Vect.scalar_mul ~name:"c0" c0 (Vect.one basis) in
  let sum = sum_of_sigma_sources basis in
  Vect.sub (Vect.add sum x_f0) c0_one

(* constructing indV and indT *)
let b3 = S.untyped_basis_id 3
let b4 = S.untyped_basis_id 4

let iT = S.id_flag b3 (Digraph.make 3 [|(0,1);(1,2);(0,2)|])
let iV = S.id_flag b3 (Digraph.make 3 [|(0,2);(1,2)|])

let ind i n =
  let basis = S.basis_id (n+1) n i in
  Vect.untype (f basis)

let indV = ind iV 3
let indT = ind iT 3

(* flags rooted on one vertex *)
let b2_1 = S.basis_id 2 1 0
let b3_1 = S.basis_id 3 1 0
let alpha = Vect.flag ~name:"alpha" b2_1 (Digraph.make 2 [|(0,1)|])
let chi = Vect.flag ~name:"chi" b3_1 (Digraph.make 3 [|(0,1);(0,2)|])
  
(* ***** forks *********** *)
 
(* Building fork *)
let kappa = Vect.flag ~name:"kappa" b3 (Digraph.make 3 [|(0,1);(0,2)|])
  
let sausage_const = 1. (*0.88*)
  
let fork = (* fork : kappa - (3*(3*c0-1)^2)/sausage *)
  let x = 3. *. c0 -. 1. in
  let y = (3. *. x*.x)/.sausage_const in
  Vect.sub kappa (Vect.scalar_mul y (Vect.one b3))

(* fork inequality : chi - (1/sausage)*(c0 - alpha)^2 *)
let rooted_fork =
  let x = Vect.sub
    (Vect.scalar_mul c0 (Vect.one b2_1))
    (Vect.expand b2_1 alpha) in
  let y = Vect.scalar_mul (1./.sausage_const) (Vect.multiply x x) in
  Vect.sub chi y
    
(* let _ = Vect.draw rooted_fork
   let _ = Vect.draw (Vect.untype rooted_fork) *)
  
(* ************ min deg *************** *)
let b_1 = { b with typeSize = 1; typeId = 0 }

let alpha_geq_c0 =
  at_least alpha c0

let alpha_geq_c0_ext =
  multiply_by_all_flags (div_basis b_1 b2_1) alpha_geq_c0

let alpha_is_c0 =
  name_list "Alpha is c0"
    (List.concat (List.map equality
		    (List.map untype alpha_geq_c0_ext)))

(* ******* Construction of the problem ********* *)
let cauchy_schwartz = all_cs b

let inequalities =
  List.concat
    [
      all_flags_nonnegative b;
      equality (totalsum b);
      [ expand b (at_least indV 0.);
	expand b (at_least indT 0.);
	expand b (at_least fork 0.) ];
      multiply_and_unlabel b (at_least rooted_fork 0.);
      alpha_is_c0
    ]

let b2 = S.untyped_basis_id 2
let edge = RatVect.flag ~name:"edge" b2 (Digraph.make 2 [|(0, 1)|])
let obj = RatVect.expand b (RatVect.scalar_mul (Rational.make (-1) 2) edge)

let _ = solve "test_" b cauchy_schwartz [] inequalities obj.vect

(*  S.basis_id *)
