module V = Vectors.Vect (Rational) (Graph)
module I = Inequality.Make (Rational) (Graph)
module S = Storage.Make (Graph)
module Solve = Solve.Make (Graph)
open Solve
open Inequality
open I

let b = S.untyped_basis_id 3
let b2 = S.untyped_basis_id 2

(* définitions *)
let triangle = V.flag b (Graph.make 3 [(0,1);(1,2);(2,0)])
let edge = V.flag b2 (Graph.make 2 [(0,1)])
let nonedge = V.flag b2 (Graph.make 2 [])		      

(* Afficher un produit *)
(*let x = V.multiply edge edge
let _ = V.draw x*)


(* Contraintes *)
let inequalities =
  List.concat
    [
      all_flags_nonnegative b; (* les drapeaux ont une valeur positive *)
      equality (totalsum b); (* la somme des drapeaux de taille 3 vaut 1 *)
      [ at_most triangle Rational.zero ] (* il n'y a pas de triangles *)
    ]
    
(* Fonction objectif : on veux minimiser moins edge *)
let obj = V.expand b (V.opposite edge)

(* Ecriture du prolème s.d.p. *)
let _ = solve "turan" b (all_cs b) inequalities [] obj.vect

