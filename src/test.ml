open Common

let grey = "\027[1;30m"
and red = "\027[1;31m"
and white = "\027[0;0m"
and blue = "\027[1;34m";;

let failed = ref 0;;

let print_time = true

let timer = ref (Sys.time ())
  
let tick () =
  let old_time = !timer in
  timer := Sys.time ();
  !timer -. old_time

let test s b =
  let time = 
    if print_time then
      let t = tick () in
      if t > 0.009 then Printf.sprintf "%s(%.2f) " blue t else "       "
    else "" in
  if not b
  then (incr failed;
	Printf.printf "%s%sTest failed : %s\n%s" time red s white)
  else Printf.printf "%s%sTest succeed : %s\n%s" time grey s white;
  flush stdout;;

let summary () =
  Printf.printf "%s%d failure%s\n"
    (if !failed > 0 then red else grey) !failed white;;

(* Combinatoric *)
open Combinatoric;;

(* fact *)
let values = [(0, 1); (1, 1); (3, 6); (5, 120)] in
List.iter (fun (n, r) -> test "fact" (fact n = r)) values;;

(* *** generic algebra tests *** *)
module GenericAlgebraTest ( Flag : Flag.S ) =
struct

  module A = Algebra.Make ( Flag )

  let test_ s = test (Printf.sprintf "%s : %s" Flag.name s)
  
  let test_nf f =
    test_ "normal_form old/new"
      (A.normal_form f = A.normal_form_old f)
      
  let test_nf_typed_1 f =
    test_ "normal_form_typed sigma=0"
      (A.normal_form f = A.normal_form_typed 0 f)
  
  let test_sg n =
    test_ (Printf.sprintf "span_flags: %d vs %d+1" n (n-1))
      (A.span_flags n = A.span_flags_next (A.span_flags (n-1)))

  let test_p2_symetric h1 h2 f =
    let nf = A.normal_form_typed 0 in
    test_ "p2 symetric"
      (A.p2 0 (nf h1) (nf h2) f = A.p2 0 (nf h2) (nf h1) f)
      
  let test_p text sigma h f res =
    test_ text (A.p sigma (A.normal_form_typed sigma h) f = res)

  let test_p2 text sigma h1 h2 f res =
    let nf = A.normal_form_typed sigma in
    let r = A.p2 sigma (nf h1) (nf h2) f in
    test_ text (r = res);;

  let test_q text sigma h res =
    test_ text (A.q sigma h = res )

  let test_span (n, r) =
    let res = List.length (A.span_flags n) in
    test_
      (Printf.sprintf "span_flags size %d, %d expected (%d found)" n r res)
      (res = r)

  (* Random tests *)
  let test_nf_random g =
    let nf = A.normal_form_typed 0 in
    let p = random_perm ( Flag.size g ) in
    test_ "random_nf" (nf g = nf (Flag.apply_morphism p g))

  let test_nf_random_all n =
    List.iter test_nf_random (A.span_flags n)

  let auto_tests_size_n i n =
    test_sg n;
    let l = A.span_flags n in
    let pick () = list_nth_first i (randomize_list l) in
    List.iter test_nf  (pick ());
    List.iter test_nf_typed_1 (pick ());
    List.iter test_nf_random (pick ())

  let auto_tests () =
    auto_tests_size_n 5 2;
    auto_tests_size_n 5 3;
(*    auto_tests_size_n 5 4; *)

end

(* generic tests for Storage.ml *)
module GenericStorageTest ( Flag : Flag.S ) =
  struct
    
    module S = Storage.Make (Flag)
    module A = Algebra.Make (Flag)

    let test_ s = test (Printf.sprintf "%s : %s" Flag.name s)

    let raw_basis0 size =
      Array.of_list (A.span_flags size)
    let raw_basis size typeSize typeId =
      match typeSize with
      | 0 -> raw_basis0 size
      | _ -> let typeFlag = (raw_basis0 typeSize).(typeId) in
	     Array.of_list (A.span_all_typed typeFlag (A.span_flags size))
      
    let test_get_basis i =
      test_ (Printf.sprintf "get_basis %d" i)
	(S.get_basis (S.untyped_basis_id i) = Array.of_list (A.span_flags i))
	
    let tabulate_p tsize tid i j =
      let scale = Combinatoric.binomial (i - tsize) (j - tsize) in
      let intify x =
	let nom = Rational.nom x
	and denom = Rational.denom x in
	assert (scale mod denom = 0);
	(scale * nom) / denom in
      Array.map
	(fun g ->
	  Array.map (fun h -> intify (A.p tsize h g)) (raw_basis i tsize tid))
	(raw_basis j tsize tid)
	
    let test_get_p tsize tid i j =
      test_ (Printf.sprintf "get_p %d in %d type%d id%d" i j tsize tid)
	(Sparse.to_dense (S.get_p (S.basis_id i tsize tid) (S.basis_id j tsize tid)) = tabulate_p tsize tid i j)

    let tabulate_p2 tsize tid i j k =
      let scale = Combinatoric.binomial (i - tsize) (k - tsize) in
      let intify x =
	let nom = Rational.nom x
	and denom = Rational.denom x in
	assert (scale mod denom = 0);
	(scale * nom) / denom in
      let ai = raw_basis i tsize tid
      and aj = raw_basis j tsize tid
      and ak = raw_basis k tsize tid in
      Array.map (fun hk -> Array.map (fun hi -> Array.map (fun hj -> intify (A.p2 tsize hi hj hk)) aj) ai) ak

(*    let print_tab s t =
      Printf.printf "Tab %s\n" s; 
      let n = Array.length t and m = Array.length t.(0) in
      for i = 0 to n-1 do
	for j = 0 to m-1 do
	  Printf.printf "%d %d : " i j;
	  Array.iter (fun q -> Printf.printf "%s " (Rational.print q)) t.(i).(j);
	  Printf.printf "\n"
	done
      done*)
	
    let test_get_p2 tsize tid i j k =
      test_ (Printf.sprintf "get_p2 %d %d %d" i j k)
	(tabulate_p2 tsize tid i j k =
	    Array.map Sparse.to_dense (S.get_p2 (S.basis_id i tsize tid) (S.basis_id j tsize tid) (S.basis_id k tsize tid)))

  end

(* generic tests for vector.ml *)
module type C = sig type t val c : t -> t -> int end
    
module GenericVectorTest ( C : C ) ( F : Field.S with type t = C.t) ( Flag : Flag.S ) =
  struct
    
    module S = Storage.Make (Flag)
    module A = Algebra.Make (Flag)
    module V = Vectors.Vect (F) (Flag)
      
    let test_ s = test (Printf.sprintf "%s : %s" Flag.name s)
      
    let b n = S.basis_id n 0 0
    let basis_size b = Array.length (V.one b).vect
    let rand_type k = Random.int (basis_size (b k))
      
    let random_coeff _ =
      F.rat (Rational.make (Random.int(5000)-10000) (Random.int(1000)+200))
 
    let count = ref (-1)
    let name () =
      incr count; String.make 1 (Char.chr ((Char.code 'x') + !count))
	
    let random b =
      { (Vectors.map random_coeff (V.one b)) with Vectors.name = Some (name ()) }

    let field_equality a b =
      C.c a b = 0
      
    let vect_equality v1 v2  =
      ( Array.length v1 = Array.length v2
      && List.for_all2 field_equality (Array.to_list v1) (Array.to_list v2))
	
    let test_eq s v1 v2 =
      count := (-1);
      let name v = match v.Vectors.name with
	| None -> "." | Some s -> s in
      let s' = Printf.sprintf "%s (%s = %s)" s (name v1) (name v2) in
      test_ s' (vect_equality v1.Vectors.vect v2.Vectors.vect)

    let test_neutral sigma k l =
      let typ = rand_type sigma in
      let b n = S.basis_id n sigma typ in
      let v = random (b l) in
      let v1 = V.multiply (V.one (b k)) v in
      let v2 = (V.expand (b (k+l-sigma)) v) in
      test_eq "neutral" v1 v2

    let test_symetry sigma k l =
      let typ = rand_type sigma in
      let b n = S.basis_id n sigma typ in
      let v = random (b k)
      and w = random (b l) in
      test_eq (Printf.sprintf "product symetry %d-%d-%d" sigma k l)
	(V.multiply v w) (V.multiply w v)
	
    let test_eq1 sigma k l =
      let typ = rand_type sigma in
      let b n = S.basis_id n sigma typ in
      let v = random (b k)
      and w = random (b l) in
      let square x = V.multiply x x in
      let v1 = square (V.sub v w)
      and v2 = V.add (V.sub (square v) (V.scalar_mul (F.int 2) (V.multiply w v))) (square w) in 
      test_eq (Printf.sprintf "equality1 %d-%d" k l) v1 v2

    let test_eq2 sigma k l =
      let typ = rand_type sigma in
      let b n = S.basis_id n sigma typ in
      let v = random (b k)
      and w = random (b l) in
      let square x = V.multiply x x in
      let v1 = V.multiply (V.sub v w) v
      and v2 = V.sub (square v) (V.multiply w v) in 
      test_eq (Printf.sprintf "Newton %d-%d" k l) v1 v2
	
    let autotest () =
      test_neutral 0 2 2;
      test_neutral 0 3 2;
      test_neutral 0 2 3;
      test_neutral 1 2 2;
      test_neutral 1 3 2;
      test_neutral 2 3 3;
      test_symetry 0 2 3;
      test_symetry 0 2 2;
      test_symetry 1 2 3;
      test_symetry 2 3 4;
      test_eq1 0 2 2;
      test_eq1 0 3 3;
      test_eq1 1 3 3;
      test_eq1 2 3 3;
      test_eq2 0 3 3;
      test_eq2 0 2 2;
      test_eq2 0 3 3;
      test_eq2 2 3 3;
  end

(* **** graph algebra **** *)
module GraphAlgebraTest = GenericAlgebraTest (Graph)
open GraphAlgebraTest;;

auto_tests ();;

let g = Graph.make 6 [(2,1);(1,5);(2,5);(1,4);(4,3)]
and g2 = Graph.make 7 [(0,1);(0,5);(1,2);(1,4);(2,3);(2,4);(5,4)]
and g3 = Graph.make 5 [(0,1);(1,2);(1,4);(2,4);(2,3)]
and e = Graph.make 2 [(0,1)]
and k3 = Graph.make 3 [(0,1);(1,2);(2,0)]
and h = Graph.make 3 [(0,2)] (* co-p3 *)
and p3 = Graph.make 3 [(0,1);(1,2)]
and p4 = Graph.make 4 [(0,1);(1,2);(2,3)]
and p5 = Graph.make 5 [(0,1);(1,2);(2,3);(3,4)]
and c5 = Graph.make 5 [(0,1);(1,2);(2,3);(3,4);(4,0)]
and g' = Graph.make 6 [(2,0);(4,1);(3,0);(3,2);(4,2)] (*g' ~ g*)
and k1 = Graph.make 1 []
and petersen = Graph.make 10 [(0,1);(1,2);(2,3);(3,4);(4,0);(0,5);(1,6);(2,7);(3,8);(4,9);(5,8);(8,6);(6,9);(9,7);(7,5)]
let all = [g;g2;g3;e;h;p3;p4;p5;k3;g';petersen];;

List.iter test_nf_typed_1 all;;
List.iter test_nf all;;

module GA = Algebra.Make (Graph);;

test "normal_form" (GA.normal_form g = GA.normal_form g');;

test "normal_form_typed g vs g'"
  (GA.normal_form_typed 1 g <> GA.normal_form_typed 1 g');;

List.iter test_span [(1,1);(2,2);(5,34)];; (* n=5 may be slow *)

(* type = 0 *)
test_p "p(k1,g)" 0 k1 g (Rational.make 1 1);;
test_p "p(e,g)" 0 e g (Rational.make 1 3);;
test_p "p(h,g)" 0 h g (Rational.make 11 20);;
test_p "p(p3,g)" 0 p3 g (Rational.make 3 20);;
test_p "p(p4,g)" 0 p4 g (Rational.make 2 15);;
test_p "p(k3,g)" 0 k3 g (Rational.make 1 20);;
test_p "p(g,g)" 0 g g (Rational.make 1 1);;
test_p "p(p3,p4)" 0 p3 p4 (Rational.make 1 2);;
test_p "p(k3,p4)" 0 k3 p4 (Rational.make 0 1);;
test_p "p(k3,petersen)" 0 k3 petersen (Rational.make 0 1);;
test_p "p(c5,petersen)" 0 c5 petersen (Rational.make 1 21);;

test_p "p_1(p3,g')" 1 p3 g' (Rational.make 1 10);;
test_p "p_1(h1,g')" 1 h g' (Rational.make 1 2);;
test_p "p_3(p4,g2')" 3 p4 g2 (Rational.make 1 4);;
test_p "p_3(g3,g2')" 3 g3 g2 (Rational.make 1 6);;
test_p "p_3(p3,g2')" 3 p3 g2 (Rational.make 1 1);;

test_p2_symetric e p3 g;;
test_p2_symetric p3 k1 g2;;
test_p2_symetric g3 e g2;;
test_p2_symetric p4 p3 g2;;

test_p2 "p(p3,e;g)" 0 p3 e g (Rational.make 1 60);;
test_p2 "p(p3,k1;g)" 0 p3 k1 g (Rational.make 3 20);;
test_p2 "p(e,e;g)" 0 e e g (Rational.make 4 45);;
test_p2 "p(h,p3;g2)" 0 h p3 g2 (Rational.make 4 35);;
test_p2 "p_3(p3,p3;g2)" 3 p3 p3 g2 (Rational.make 1 1);;
test_p2 "p_3(p3,p4;g2)" 3 p3 p4 g2 (Rational.make 1 4);;
test_p2 "p_3(p4,p4;g2)" 3 p4 p4 g2 (Rational.make 0 1);;
test_p2 "p_3(p4,p5;petersen)" 3 p4 p5 petersen (Rational.make 2 105);;
test_p2 "p_2(h,h;g')" 2 h h g' (Rational.make 1 6);;

test_q "q_1(p3)" 1 p3 (Rational.make 2 3);;
test_q "q_1(petersen)" 1 petersen (Rational.make 1 1);;
test_q "q_2(petersen)" 2 petersen (Rational.make 1 3);;
test_q "q_3(petersen)" 3 petersen (Rational.make 1 12);;
test_q "q_2(k3)" 2 k3 (Rational.make 1 1);;
test_q "q_2(c5)" 2 p5 (Rational.make 1 10);;
test_q "q_2(g2)" 2 g2 (Rational.make 1 21);;
test_q "q_3(p4)" 3 p4 (Rational.make 1 12);;
test_q "q_3(g')" 3 g (Rational.make 1 60);;
test_q "q_6(g)" 6 g (Rational.make 1 360);;

(* tests of storage *)

module GraphStorageTest = GenericStorageTest (Graph)
open GraphStorageTest;;

List.iter test_get_basis [ 3; 4 ];;

test_get_p 0 0 1 2;;
test_get_p 0 0 1 3;;
test_get_p 0 0 1 4;;
test_get_p 0 0 2 3;;
test_get_p 0 0 2 4;;
test_get_p 1 0 2 4;;
test_get_p 2 0 2 4;;
test_get_p 2 1 2 4;;

test_get_p2 0 0 1 4 5;;
test_get_p2 0 0 2 3 5;;
test_get_p2 0 0 2 2 4;;
test_get_p2 1 0 2 2 3;;
test_get_p2 1 0 2 3 4;;
test_get_p2 2 0 3 3 4;;
(* test_get_p2 2 1 4 3 5;; *)

module DigraphStorageTest = GenericStorageTest (Digraph)
open DigraphStorageTest;;

List.iter test_get_basis [ 3; 4 ];;

test_get_p 0 0 1 2;;
test_get_p 0 0 1 3;;
test_get_p 0 0 1 4;;
test_get_p 0 0 2 3;;
test_get_p 0 0 2 4;;
test_get_p 1 0 2 3;;
test_get_p 2 0 2 4;;
test_get_p 2 1 2 4;;

test_get_p2 0 0 1 3 4;;
test_get_p2 0 0 2 2 4;;
test_get_p2 1 0 2 2 3;;
test_get_p2 1 0 2 3 4;;
test_get_p2 2 0 3 3 4;;

module TrianglefreeStorageTest = GenericStorageTest (Trianglefree)
open TrianglefreeStorageTest;;

List.iter test_get_basis [ 3; 4 ];;

test_get_p 0 0 1 2;;
test_get_p 0 0 1 3;;
test_get_p 0 0 1 4;;
test_get_p 0 0 2 3;;
test_get_p 0 0 2 4;;
test_get_p 1 0 2 4;;
test_get_p 2 0 3 4;;
test_get_p 2 1 2 4;;

test_get_p2 0 0 1 3 4;;
test_get_p2 0 0 2 2 4;;
test_get_p2 1 0 2 2 3;;
test_get_p2 1 0 2 3 4;;
test_get_p2 2 0 3 3 4;;

(* ***** tests of Vectors ******** *)
module CNum = struct type t = Num.num let c = Num.compare_num end
module GraphVectorTest = GenericVectorTest (CNum) (Field.Num) (Graph)
open GraphVectorTest;;

autotest ()

module TrianglefreeVectorTest = GenericVectorTest (CNum) (Field.Num) (Trianglefree)
open TrianglefreeVectorTest;;
(*
autotest ()
*)

(* ***** tests of digraphs ***** *)
module DigraphAlgebraTest = GenericAlgebraTest (Digraph)

let _ = DigraphAlgebraTest.auto_tests ()
let _ = List.iter DigraphAlgebraTest.test_span [(1,1);(2,2);(3,7)];;
  
(* ***** tests of digraphs without trianlge ***** *)
module TriangleFreeAlgebraTest = GenericAlgebraTest (Trianglefree)

let _ = TriangleFreeAlgebraTest.auto_tests ()
let _ = List.iter TriangleFreeAlgebraTest.test_span [(1,1);(2,2);(3,6)]

(* *****  ***** *)

module Graph_modAlgebraTest = GenericAlgebraTest (Graph_mod)

let _ = Graph_modAlgebraTest.auto_tests ()
let _ = List.iter Graph_modAlgebraTest.test_span [(1,4)]

let _ = summary ();;
