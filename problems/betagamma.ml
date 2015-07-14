module S = Storage.Make (Trianglefree)
module I = Inequality.Make (Rational) (Trianglefree)
module If = Inequality.Make (Field.Float) (Trianglefree)
module Solve = Solve.Make (Trianglefree)
module V = Vectors.Vect (Rational) (Trianglefree)
module Vf = Vectors.Vect (Field.Float) (Trianglefree)

open Solve

let pf name float =
  Print.p ~color:Print.red (Printf.sprintf "%s = %f\n" name float)  
(*  *)
open Inequality
open I

(* parameters *)
let param = Rational.make 87 100
let alpha = Rational.div (Rational.int 1) param
let param0 = Rational.to_float param
let alph = Rational.to_float alpha
	   
let b31 = S.basis_id 3 1 0
let b21 = S.basis_id 2 1 0
let b = S.untyped_basis_id 3
let b3 = S.untyped_basis_id 3
let b_1 = S.basis_id 3 1 0

(* 3-digraphs rooted on a point *)
let p3a = V.flag ~name:"p3a" b31 (Digraph.make 3 [|(0,1);(1,2)|])
let p3b = V.flag ~name:"p3b" b31 (Digraph.make 3 [|(2,0);(0,1)|])
let p3c = V.flag ~name:"p3c" b31 (Digraph.make 3 [|(1,2);(2,0)|])
let e3a = V.flag ~name:"e3a" b31 (Digraph.make 3 [||])
let ta = V.flag ~name:"ta" b31 (Digraph.make 3 [|(1,2)|])
let tb = V.flag ~name:"tb" b31 (Digraph.make 3 [|(0,1)|])
let tc = V.flag ~name:"tc" b31 (Digraph.make 3 [|(1,0)|])

let edge = V.flag ~name:"e" (S.untyped_basis_id 2)
		  (Digraph.make 2 [|(1,0)|])
let trans = V.flag ~name:"trans" (S.untyped_basis_id 3)
		  (Digraph.make 3 [|(0,1);(1,2);(0,2)|])
let k2a = V.flag ~name:"k2a" b21
		 (Digraph.make 2 [|(1,0)|])
let e2a = V.flag ~name:"e2a" b21
		 (Digraph.make 2 [||])

(* let _ =
  let x = V.sub k2a e2a in
  V.draw (V.untype (V.multiply x x))*)

(*
let _ = V.draw (V.one (S.basis_id 4 2 1))
 *)
		
let t = V.flag ~name:"t" b3 (Digraph.make 3 [|(1,0)|])
		 
let e n =  V.flag ~name:(Printf.sprintf "e%d" n)
		  (S.untyped_basis_id n) (Digraph.make n [||])
let e3 = e 3

		
let r i j v = V.scalar_mul (Rational.make i j) v

(* random cut *)
let random =
  let decycl =
    List.fold_left V.add (V.empty b31)
		   [ r 1 2 p3a;
		     r 1 2 p3c;
		     r 1 4 ta ] in
  let missing =
    List.fold_left V.add (V.empty b31)
		   [ r 1 1 p3b;
		     r 1 2 tb;
		     r 1 2 tc;
		     r 1 2 e3a ] in
  less_than (V.scalar_mul ~name:"param" param missing) decycl

(* majority cut *)
let to_float = Vectors.map Rational.to_float
	    
let maj =
  let missing = to_float p3b in
  let decycl0 = to_float (V.scalar_mul (Rational.int 2) (V.multiply e2a e2a)) in
  let z = (alph +. (sqrt(alph*.(alph +. 1.))))*.0.5 in
  let mu = alph -. 1. in
  pf "param" param0;
  pf "alpha" alph;
  pf "mu" mu;
  pf "z" z;
  pf "P(z)" (((z -. alph)*.z) -. 0.25*.alph);
  pf "1/(4mu) - (1+mu)(z+1/4)" (0.25/.mu -. (1.+.mu)*.(z+.0.25)); 
  let decycl = Vf.scalar_mul (alph *. (0.25 +. z)) decycl0 in
  If.less_than missing decycl

(* ***** type cut ***** *)
let array_count f t =
  let rec aux r i =
    if i < 0 then r
    else aux (if f t.(i) then r+1 else r) (i-1) in
  aux 0 ((Array.length t) - 1)
	       
(* compute the number of arcs leaving the type *)
let out_type t flag =
  let cond (i, j) = i < t && t <= j in
  array_count cond flag.Digraph.e
	       
(* compute the number of arcs entering the type *)
let in_type t flag =
  let cond (i, j) = j < t && t <= i in
  array_count cond flag.Digraph.e
	       
(* compute the number of non-edges between the type and the rest *)
let nonedge_type t flag =
  let n = flag.Digraph.n in
  t*(n-t) - (in_type t flag) - (out_type t flag)
				 
let typecut b =
  let aux f t flag = Field.Float.int (f t flag) in
  let out = Vf.make ~name:"out" b (aux out_type) in
  let in_ = Vf.make ~name:"in" b (aux in_type) in
  let non = Vf.make ~name:"missing" b (aux nonedge_type) in
  let non2 = Vf.scalar_mul ~name:"param" (1./.alph) non in
  [ If.at_least (Vf.sub out non2) 0.;
    If.at_least (Vf.sub in_ non2) 0. ]
	       
let all_typecuts b =
  let n = b.Storage.flagSize in
  let res = ref [] in
  for typeSize = 1 to n-1 do
    let m = S.get_size { b with Storage.flagSize = typeSize } in
    for i = 0 to m - 1 do
      let new_b = { b with Storage.typeSize = typeSize;
			   typeId = i } in
      res := (typecut new_b) :: !res
    done
  done;
  List.concat !res

(* let a x =  Vf.draw x.Inequality.flags

let _ = List.iter a (all_typecuts b)*)
(*
  let typecut_coeff flag t =
 *) 

(* **** classification cuts ****)
type adj = Forward | Backward | Nonedge

let adjacency flag i j =
  let res = ref Nonedge in
  Array.iter (fun x -> if x = (i, j) then res := Forward
		       else if x = (j, i)
		       then res := Backward) flag.Digraph.e;
  !res
	      
let classify i t flag =
  let n = flag.Digraph.n in
  let subset = Array.init (t+1) (fun x -> x) in
  subset.(t) <- i;
  let induced = Digraph.induce subset flag in
  if out_type t induced <= in_type t induced
  then 1
  else 2

let class1 t flag =
  let n = flag.Digraph.n in
  assert (n = t + 2);
  let c1 = classify t t flag in
  let c2 = classify (t+1) t flag in
  match adjacency flag t (t+1) with
  | Nonedge -> if c1 <> c2 then Rational.minus param else Rational.zero
  | Forward -> if c1 > c2 then Rational.int 1 else Rational.zero
  | Backward -> if c1 < c2 then Rational.int 1 else Rational.zero

let toto = (V.make (S.basis_id 3 1 0) class1)
let tata = V.untype toto
(*let _ = V.draw toto
let _ = V.draw tata*)
        
let cauchy_schwartz = all_cs b

(* let _ = V.draw random.flags *)
			     
let inequalities =
  List.concat
    [
      [ at_least (V.expand b tata) Rational.zero ];
      multiply_and_unlabel b random;
      all_flags_nonnegative b;
      equality (totalsum b);
    ]
			     
let inequalities_float =
  List.concat
    [
      If.multiply_and_unlabel b maj;
      (*   List.map If.untype (all_typecuts b);*)
    ]
    
let obj = V.expand b (V.opposite (e 3))
		   
(* let _ = V.draw obj *)
let f i = V.draw (V.untype i.flags);
	  Print.p ~color:Print.red (Printf.sprintf "= %s\n" (Rational.print i.bound))
let g i = Vf.draw (Vf.untype i.flags) ; pf "" i.bound; flush stdout

let _ = List.map f inequalities
							     
(*let _= f (List.hd inequalities)
let _ = List.iter g inequalities_float*)
	       
let _ = solve "test_a" b cauchy_schwartz inequalities inequalities_float obj.vect

(* let a = V.expand (S.basis_id 4 1 0) p3b

let _ = V.draw a
 *)
