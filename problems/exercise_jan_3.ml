(* triangle = 0 *)
(* maximize c5 *)

module S = Storage.Make (Graph)
module I = Inequality.Make (Graph)

(*  *)
open Inequality
open I

let b3 = S.untyped_basis_id 3
let b = S.untyped_basis_id 5

let cauchy_schwartz = all_cs b

let triangle = S.id_flag b3 (Graph.make 3 [(0,1);(1,2);(2,0)])
let c5 = S.id_flag b (Graph.make 5 [(0,1);(1,2);(2,3);(3,4);(4,0)])

let inequalities =
  List.concat
    [
      all_flags_nonnegative b;
      equality Rational.minus (totalsum b);
      equality Rational.minus 
	(ineq_expand b (flag_at_least b3 triangle (Rational.int 0)))
    ]

let obj = empty_sum b

let _ = obj.(c5) <- Rational.int (-1)

let _ = solve "test_a" b cauchy_schwartz inequalities obj
