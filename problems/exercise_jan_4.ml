(* Problem 4 : Turan *)
(* k4 = 0 *)
(* maximize edge *)

module S = Storage.Make (Graph)
module I = Inequality.Make (Graph)

(*  *)
open Common
open Inequality
open I

let size2 = S.untyped_basis_id 2
let size4 = S.untyped_basis_id 4

let edge = S.id_flag basisSize2 (Graph.make 2 [(0,1)])
let k4 = S.id_flag basis (Graph.make 4 [(0,1);(0,2);(0,3);(1,2);(1,3);(2,3)])

let cauchy_schwartz = all_cs size4

let inequalities =
  List.concat
    [
      all_flags_nonnegative size4;
      equality (totalsum size4);
      expand b (flag_at_most size4 k4 zero)
    ]

let objective = Vect.expand size4 (flag_of_id size2 edge)

let _ = solve "turan" b cauchy_schwartz inequalities objective
