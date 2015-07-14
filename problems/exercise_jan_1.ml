(* triangle >= 1/2 *)
(* maximize independant set of size 3 *)

module S = Storage.Make (Graph)
module I = Inequality.Make (Graph)

(*  *)
open Inequality
open I

let b = S.untyped_basis_id 3

let cauchy_schwartz = all_cs b

let triangle = S.id_flag b (Graph.make 3 [(0,1);(1,2);(2,0)])
let cotriangle = S.id_flag b (Graph.make 3 [])

let inequalities =
  List.concat
    [
      all_flags_nonnegative b;
      equality Rational.minus (totalsum b);
      [ flag_at_least b triangle (Rational.make 1 2) ]
    ]

let obj = empty_sum b

let _ = obj.(cotriangle) <- Rational.int (-1)

let _ = solve "test_a" b cauchy_schwartz inequalities obj
