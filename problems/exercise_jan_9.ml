(* Problem 8 *)
(* maximize digraph of size 3 with one edge *)

module S = Storage.Make (Digraph)
module I = Inequality.Make (Digraph)

(*  *)
open Common
open Inequality
open I

(* shortcuts *)
let equality i = equality Rational.minus i
let opposite i = [opposite Rational.minus i]

let b = S.untyped_basis_id 3

let cauchy_schwartz = all_cs b

let c3 = S.id_flag b (Digraph.make 3 [|(0,1)|])

let inequalities =
  List.concat
    [
      all_flags_nonnegative b;
      equality (totalsum b)
    ]

let obj = empty_sum b

let _ = obj.(c3) <- Rational.int (-1)

let _ = solve "test_" b cauchy_schwartz inequalities obj
