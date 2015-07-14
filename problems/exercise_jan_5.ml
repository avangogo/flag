(* Problem 5 *)
(* no P5 as subgraph *)
(* maximize edge *)

module S = Storage.Make (Graph)
module I = Inequality.Make (Graph)

(*  *)
open Common
open Inequality
open I

(* shortcuts *)
let equality i = equality Rational.minus i
let opposite i = [opposite Rational.minus i]

let b2 = S.untyped_basis_id 2
let b5 = S.untyped_basis_id 5
let b = S.untyped_basis_id 6

let cauchy_schwartz = all_cs b

let edge = S.id_flag b2 (Graph.make 2 [(0,1)])

let c5_edges = [(0,1);(1,2);(2,3);(3,4);(4,0)]
let other_edges = [(0,2);(1,3);(2,4);(3,0);(4,1)]
let c5_as_subgraph =
  let make e =
    S.id_flag b5 (Graph.make 5 (e @ c5_edges)) in
  let l = List.map make (Combinatoric.all_subsets other_edges) in
  remove_duplicated l

let _ =
  let b5_ = S.get_basis b5 in
  Graphic.draw_list (fun i -> Graph.draw (b5_.(i))) c5_as_subgraph

let flag5_equal_zero f =
  Inequality.opposite Rational.minus
    (expand b (flag_at_least b5 f Rational.zero))

let inequalities =
  List.concat
    [
      all_flags_nonnegative b;
      equality (totalsum b);
      List.map flag5_equal_zero c5_as_subgraph
    ]

let obj2 = empty_sum b2

let _ = obj2.(edge) <- Rational.int (-1)

let obj = vect_expand b2 b obj2

let _ = solve "test_" b cauchy_schwartz inequalities obj
