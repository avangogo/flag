(* triangle = 0 *)
(* minimize independant set of size 4 *)

module S = Storage.Make (Graph)
module I = Inequality.Make (Graph)

(*  *)
open Inequality
open I

(* shortcuts *)
let equality i = equality Rational.minus i
let opposite i = [opposite Rational.minus i]

let b3 = S.untyped_basis_id 3
let b4 = S.untyped_basis_id 4
let b = S.untyped_basis_id 5

let cauchy_schwartz = all_cs b

let triangle = S.id_flag b3 (Graph.make 3 [(0,1);(1,2);(2,0)])
let cok4 = S.id_flag b4 (Graph.make 4 [])


let inequalities =
  List.concat
    [
      all_flags_nonnegative b;
      equality (totalsum b);
      opposite (expand b (flag_at_least b3 triangle Rational.zero))
    ]

let obj4 = empty_sum b4

let _ = obj4.(cok4) <- Rational.int 1

let obj = vect_expand b4 b obj4

let _ = solve "test_" b cauchy_schwartz inequalities obj
