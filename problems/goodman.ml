module S = Storage.Make (Graph)
module I = Inequality.Make (Rational) (Graph)
module Solve = Solve.Make (Graph)
module Vect = Vectors.Vect (Rational) (Graph)

open Solve
open Inequality
open I

let b_2_point_rooted = S.basis_id 2 1 0
let b_3 = S.untyped_basis_id 3

let cauchy_schwartz = [ b_2_point_rooted ]

let inequalities =
  List.concat
    [
      all_flags_nonnegative b_3;
      equality (totalsum b_3);
    ]

let triangle = Vect.flag ~name:"k3" b_3 (Graph.make 3 [(0,1);(1,2);(2,0)])
let cotriangle = Vect.flag ~name:"e3" b_3 (Graph.make 3 [])
    
let obj = Vect.add triangle cotriangle

let _ = solve "goodman" b_3 cauchy_schwartz inequalities [] obj.vect
  
