(* Caccetta-Haggkvist *)

open Digraph
open Storage
module S = Storage.Make (Trianglefree)
module I = Inequality.Make (Field.Float) (Trianglefree)
module V = Vectors.Vect (Field.Float) (Trianglefree)
module Problem = Problem.Make (Field.Float) (Trianglefree)
open V
open I
     
(* **** Parameters **** *)
let flagSize = 6 (* Size of the flags used *)
let c  = 0.3392  (* Constant for which we prove the result *)
let c1 = 0.3465  (* Constant for which we know it holds *)

(* Basis on which we build the sdp problem *)
let b = S.basis_id flagSize
           
(* **** Computing Inequalities of Theorem %%\ref{thm:CH:inequalities}% **** *)
                   
(* ==== 1. Degree is c ==== *)

let outedge =
  let basis2_1 = S.basis_id ~typeSize:1 ~typeId:0 2 in
  V.flag ~name:"outedge" basis2_1 (Digraph.make 2 [|(0,1)|])
         
let outdegree_geq_c = { ( at_least outedge c )
                      with boundName = Some "c" }
                               
let outdegree_is_c =
  let ineqs = multiply_and_unlabel b (outdegree_geq_c) in
  name_list "1. Outdegree is c"
    (List.concat (List.map (equality ~epsilon:1e-11) ineqs))
                   
(* ==== 2. Density of forks is at least 3(3c-1)^2/a ==== *)

(* Constant for the Chudnovsky-Seymour-Sullivan conjecture *)
let a = 0.88
          
(* basis of flags of size 3 *)
let b3 = S.basis_id 3
          
(* fork *)
let fork =
  V.flag ~name:"fork" b3 (Digraph.make 3 [|(0,1);(0,2)|])

let fork_ineq =
  let x = 3. *. c -. 1. in
  let y = (3. *. x *. x) /. a in
  { ( at_least fork y )
  with name = Some "2. Fork" ;
       boundName = Some "3*(3c-1)^2/a" }
    
(* ==== 3. [|f(sigma)F|] >= 0 for every sigma-source ==== *)
            
(* Determine if the flag in input is a sigma-source *)
let is_a_sigma_source typeSize flag =
  let edge_condition (i, j) =
    not (i >= typeSize && j < typeSize) in
  Common.array_for_all edge_condition flag.e

(* Return the sum of sigma sources of the basis in input *)
let sum_of_sigma_sources basis =
  let indicator typeSize f =
    if is_a_sigma_source typeSize f then 1. else 0. in
  V.make ~name:"Sigma sources" basis indicator
         
(* Build the graph f0 obtained by adding a sink to sigma *)
let f0_flag sigma_flag =
  let n = sigma_flag.n in
  let new_edges =  Array.init n (fun i -> (i, n)) in
  let edges = Array.concat [sigma_flag.e; new_edges] in
  Digraph.make (n+1) edges

(* Recover the type of this basis in input (as a graph) *)
let get_type basis =
  ( S.get_basis (S.basis_id basis.typeSize) ).(basis.typeId)

(* Return the vector corresponding to f0 *)
let f0 basis =
  let f0 = f0_flag (get_type basis) in
  V.flag ~name:"F0" basis f0
         
(* Compute f(sigma) = sum of sigma-sources + (c1-1)F0 - c *)
let f basis =
  let x_f0 = scalar_mul ~name:"(c1-1)" (c1-.1.) (f0 basis) in
  let c_one = scalar_mul ~name:"c" c (one basis) in
  let sum = sum_of_sigma_sources basis in
  sum +~ x_f0 -~ c_one   (* f(sigma) *)

(* f(sigma) >= 0 *)
let f_inequality basis = at_least (f basis) 0.
           
let has_dominated_vertex g =
  Common.array_exists ( (==) (g.n - 1) ) (in_degrees g)

(* Build the inequalities of the type f(sigma) >= 0 *)
(* for the types sigma with a dominated vertex *)
let f_rooted_ineqs =
  let res = ref [] in
  for n = 2 to b.flagSize - 1 do
    let types = S.get_basis (S.basis_id n) in
    for i = 0 to (Array.length types) - 1 do
      if has_dominated_vertex types.(i) then begin
        let basis = S.basis_id ~typeSize:n ~typeId:i (n+1) in
        res := ( f_inequality basis ) :: !res
       end
    done
  done;
  !res
   
(* Build the inequalities of the type [|f(sigma)*F|] >= 0 *)
let f_inequalities =
  name_list "3. Sigma-sources"
    (List.concat
      (List.map (multiply_and_unlabel b) f_rooted_ineqs))
    
(* **** Construction of the problem **** *)

(* List of inequalties used *)     
let inequalities =
  List.concat
    [
      all_flags_nonnegative b;  (* A flag is non-negative *)
      [ at_least (one b) 1. ];  (* Sum of flags at least 1 *)
      outdegree_is_c;           (* Constraint 1. *)
      [ expand b (fork_ineq) ]; (* Constraint 2. *)
      f_inequalities;           (* Constraint 3. *)
    ]

(* Print the sdp program in CH.sdpa *)
let _ = Problem.write "CH" inequalities (one b)
