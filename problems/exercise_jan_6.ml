(* triangle = 0 *)
(* minimize independant set of size 4 *)
(* ( The answer should be 3/25 ) *)
open Vectors
module S = Storage.Make (Graph)
module I = Inequality.Make (Rational) (Graph)
module Vect = Vectors.Vect (Rational) (Graph)
module Solve = Solve.Make (Graph)
open Solve
open I


let b3 = S.untyped_basis_id 3
let b4 = S.untyped_basis_id 4
let b = S.untyped_basis_id 5

let cauchy_schwartz = all_cs b

let triangle = Vect.flag ~name:"k3" b3 (Graph.make 3 [(0,1);(1,2);(2,0)])
let cok4 = Vect.flag ~name:"cok4" b4 (Graph.make 4 [])

let inequalities =
  List.concat
    [
      all_flags_nonnegative b;
      equality (totalsum b);
      [opposite (expand b (at_least triangle Rational.zero))]
    ]

let obj = Vect.expand b cok4

let _ = solve "test_" b cauchy_schwartz inequalities [] obj.vect
