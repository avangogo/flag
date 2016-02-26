(* triangle >= 1/2 *)
(* maximize independant set of size 3 *)

module S = Storage.Make (Graph)
module I = Inequality.Make (Rational) (Graph)
module Solve = Solve.Make (Graph)
module Vect = Vectors.Vect (Rational) (Graph)
  
(*  *)
open Solve
open Inequality
open I

let b = S.untyped_basis_id 3

let cauchy_schwartz = all_cs b

let _ = Vect.draw (Vect.one b)
  
let triangle = S.id_flag b (Graph.make 3 [(0,1);(1,2);(2,0)])
let cotriangle = Graph.make 3 []

let inequalities =
  List.concat
    [
      all_flags_nonnegative b;
      equality (totalsum b);
      [ flag_at_least b triangle (Rational.make 1 2) ]
    ]

let obj = Vect.opposite ( Vect.flag ~name:"E3" b cotriangle )

let _ = solve "turan" b cauchy_schwartz inequalities [] obj.vect
