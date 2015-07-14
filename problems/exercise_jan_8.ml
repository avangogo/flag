(* Problem 7 *)
(* maximize transitive triangle *)

module S = Storage.Make (Digraph)
module I = Inequality.Make (Digraph)

(*  *)
open Common
open Inequality
open I

(* shortcuts *)
let equality i = equality Rational.minus i
let opposite i = [opposite Rational.minus i]

let b3 = S.untyped_basis_id 3
let b = S.untyped_basis_id 3

let cauchy_schwartz = all_cs b

let c3 = S.id_flag b3 (Digraph.make 3 [|(0,1);(1,2);(2,0)|])

let inequalities =
  List.concat
    [
      all_flags_nonnegative b;
      equality (totalsum b)
    ]

let obj3 = empty_sum b3

let _ = obj3.(c3) <- Rational.int (-1)

let obj = vect_expand b3 b obj3

let _ = solve "test_" b cauchy_schwartz inequalities obj
