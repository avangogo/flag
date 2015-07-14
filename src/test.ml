open Common

let grey = "\027[1;30m"
and red = "\027[1;31m"
and white = "\027[0;0m";;

let failed = ref 0;;

let test s b =
  if not b
  then (incr failed;
	Printf.printf "%sTest failed : %s\n%s" red s white)
  else Printf.printf "%sTest succeed : %s\n%s" grey s white;
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
    auto_tests_size_n 5 4;

end

(* generic tests for Storage.ml *)
module GenericStorageTest ( Flag : Flag.S ) =
  struct
    
    module S = Storage.Make (Flag)
    module A = Algebra.Make (Flag)

    let test_ s = test (Printf.sprintf "%s : %s" Flag.name s)

    let test_get_basis i =
      test_ (Printf.sprintf "get_basis %d" i)
	(S.get_basis (S.untyped_basis_id 3) = Array.of_list (A.span_flags 3))

    let tabulate_p i j =
      Array.map
	(fun h -> Array.map (A.p 0 h) (Array.of_list (A.span_flags j))) 
	(Array.of_list (A.span_flags i))
	
    let test_get_p i j =
      test_ (Printf.sprintf "get_p %d %d" i j)
	(S.get_p (S.untyped_basis_id i) (S.untyped_basis_id j) = tabulate_p i j)

    let tabulate_p2 i j k =
      let ai = Array.of_list (A.span_flags i)
      and aj = Array.of_list (A.span_flags j)
      and ak = Array.of_list (A.span_flags k) in
      Array.map (fun hi -> Array.map (fun hj -> Array.map (fun hk -> A.p2 0 hi hj hk) ak) aj) ai

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
	
    let test_get_p2 i j k =
      test_ (Printf.sprintf "get_p2 %d %d %d" i j k)
	(tabulate_p2 i j k = S.get_p2 (S.untyped_basis_id i) (S.untyped_basis_id j) (S.untyped_basis_id k))

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

test_get_p 1 2;;
test_get_p 1 3;;
test_get_p 1 4;;
test_get_p 2 3;;
test_get_p 2 4;;
test_get_p2 1 4 5;;
test_get_p2 2 3 5;;
test_get_p2 2 2 4;;

(* ***** tests of digraphs ***** *)
module DigraphAlgebraTest = GenericAlgebraTest (Digraph)

let _ = DigraphAlgebraTest.auto_tests ()
let _ = List.iter DigraphAlgebraTest.test_span [(1,1);(2,2);(3,7)]

(* ***** tests of digraphs without trianlge ***** *)
module TriangleFreeAlgebraTest = GenericAlgebraTest (Trianglefree)

let _ = TriangleFreeAlgebraTest.auto_tests ()
let _ = List.iter TriangleFreeAlgebraTest.test_span [(1,1);(2,2);(3,6)]

(* *****  ***** *)

module Graph_modAlgebraTest = GenericAlgebraTest (Graph_mod)

let _ = Graph_modAlgebraTest.auto_tests ()
let _ = List.iter Graph_modAlgebraTest.test_span [(1,4)]

let _ = summary ();;
