open Vectors
open Storage
module S = Storage.Make (Graph_mod)
module I = Inequality.Make (Rational) (Graph_mod)
module Vect = Vectors.Vect (Rational) (Graph_mod)
module Solve = Solve.Make (Graph_mod)
open Solve
open I

let b = S.untyped_basis_id 4

let cauchy_schwartz = all_cs b

let complement i j =
  match List.filter (fun k -> k <> i && k <> j) [2;3;4;5] with
  | [a;b] -> (a,b)
  | _ -> failwith "toto"

let nb_c6 g =
  assert (Graph.size g = 6);
  let nbrs =
    Array.map (List.filter (fun i -> i >= 2))
      (Array.init 6 (Graph.neibrs g)) in
  let in_E (i,j) = List.mem i nbrs.(j) in
  let res = ref 0 in
  let treat a b =
    if a <> b then
      let c, d = complement a b in
      if in_E (c, d) then
	begin
	  if in_E (a, c) && in_E (d, b) then incr res;
	  if in_E (a, d) && in_E (c, b) then incr res
	end in
  List.iter (fun i -> List.iter (fun j -> treat i j) nbrs.(0)) nbrs.(1);
  !res

(* degree min *)
let k = 10

let degree sigma g =
  assert (Graph.size g = 4);
  assert (sigma = 2);
  if List.mem 4 (Graph.neibrs g 3)
  then Rational.int 1
  else Rational.int 0

let b3 = untyped_basis_id 3

let degree_ineq typ =
  let b4_t = basis_id 4 3 typ in
  let deg = Vect.make ~name:"degree" b4_t degree in
  let ineq = at_most deg (Rational.make 1 k)

let degree_ineqs =
  list_init (S.get_size b3) degree_ineq



(*let product i =
  let x = (Vect.flag_of_id ~name:"x" b3_1 i) in
  Vect.untype (Vect.multiply alpha_minus_c0 x)

let products = Common.list_init (S.get_size b3_1) product

let alpha_is_c0 =
  List.concat
    (List.map equality (List.map (fun v -> at_least v 0.) products))*)
(*let _ = Graphic.draw_list Graph.draw (Array.to_list (S.get_basis b))*)

(*let edge = RatVect.flag ~name:"edge" b2 (Digraph.make 2 [|(0, 1)|])
let obj = RatVect.expand b (RatVect.scalar_mul (Rational.make (-1) 2) edge)*)


let obj = Vect.make b (fun _ g -> Rational.int (-(nb_c6 g)))

(*let _ =
  let n = Array.length opt.vect in
  for i = 60 to n-1 do
    obj.vect.(i) <- Rational.zero
  done

let _ = Vect.draw obj    *)

let inequalities =
  List.concat
    [
      all_flags_nonnegative b;
      equality (totalsum b);
    ]


let _ = solve "test_" b cauchy_schwartz inequalities [] obj.vect
