module S = Storage.Make (Graph) (Rational)
module I = Inequality.Make (Graph) (Rational)

open Inequality
open I

let b_2_point_rooted = S.basis_id 2 1 0
let b_size_3 = S.untyped_basis_id 3

let cauchy_schwartz = [ b_2_point_rooted ]

let inequalities =
  List.concat
    [equality Rational.minus (totalsum b_size_3);
     List.map (flag_nonnegative b_size_3) [0;1;2;3]]

let obj = [|Rational.int 1; Rational.int 1; Rational.int 0; Rational.int 0|]
(* k_3 + co-K_3 *) 

let _ = solve "goodman" b_size_3 cauchy_schwartz inequalities obj
