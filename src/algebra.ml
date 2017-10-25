open Common
open Combinatoric

module Make ( Flag : Flag.S ) =
struct
  
  type flag = Flag.t

  let flagname = Flag.name

  (******* Normal form algorithms ********)

  (* ** Operations on partition refinement structures ** *)
    
  let is_discrete pi =
    Refine.total_size pi == Refine.sets pi
      
  let choose_partition pi =
    let min = ref max_int
    and s_min = ref (-1) in
    for s = 0 to (Refine.sets pi) - 1 do
      let size = Refine.size pi s in
      if  size >= 2 && size < !min  then
	begin
	  min := size;
	  s_min := s
	end
    done;
    !s_min
      
  let choose_partition_list pi =
    let res = ref [] in
    let s = choose_partition pi in
    Refine.iter pi (fun e -> res := e::(!res)) s;
    !res
      
  let individualize old_pi e =
    let pi = Refine.copy old_pi in 
    Refine.mark pi e;
    ignore (Refine.split pi (Refine.set pi e) : int);
    pi
      
  let perm_of_refine pi =
    assert (is_discrete pi);
    Array.init (Refine.total_size pi) (Refine.set pi)
      
  let apply pi g =
    Flag.apply_morphism (perm_of_refine pi) g
      
  (* ** refining algorithm ** *)
      
  (* given a list of lists [l1,...,ln] (for instance a list of neibourhoods of a set S), returns a partition (as a list of list) of the elements of these lists according to their number of occurence in the input list (in our example according to their degree against S) *)
  let partition neibourghoods =
    let l = List.map makeMultiset neibourghoods in
    let multi = List.fold_left mergeMultiset [] l in
    List.map snd (makeAssoc multi)
      
  (* cons_array [(|h1; ... ;hn|] as t) [|t1; ... ;tn|] returns [|t1::h1; ... ;tn::hn|] *)
  (* the arrays must have the same length *)
  (* !!! modifies directly tl_vect !!! *)
  let cons_array hd_vec tl_vect =
    for i = 0 to (Array.length tl_vect) - 1 do
      tl_vect.(i) <- hd_vec.(i) :: tl_vect.(i)
    done
      
  let precompute_invariant g =
    let n = Flag.size g in
    Array.init n (Flag.iso_invariant g)
      
  (* refine pi according in a canonical (isomorphism-invarriant) way
     as far as it is possible with help of provided invariants of g *)
  let equitable_partition pi g =
    let invariant = precompute_invariant g in
    let stack = Stack.create () in
    for i = 0 to (Refine.sets pi) - 1 do
      Stack.push i stack
    done;
    while not (is_discrete pi) && not (Stack.is_empty stack) do
      let s = Stack.pop stack in
      let neibrs =
	let res = Array.make Flag.invariant_size [] in
	Refine.iter pi (fun x -> cons_array invariant.(x) res) s;
	res in
      let p = Array.map partition neibrs in
      for i = 0 to Flag.invariant_size - 1 do
	List.iter (Refine.cut pi (fun s -> Stack.push s stack)) p.(i)
      done
    done;
    pi
      
  (* ** basic normal form algorithm ** *)
      
  let rec normal_form_constraint_old g pi =
    let nf e =
      let pi_e = equitable_partition (individualize pi e) g in
      normal_form_constraint_old g pi_e in
    if is_discrete pi then
      apply pi g
    else
      begin
	let s = choose_partition pi in
	let e = ref (Refine.first pi s) in
	let best_nf = ref (nf !e) in
	try
	  while e := Refine.next pi !e; !e <> -1 do
	    let nf_e = nf !e in
	    if nf_e > !best_nf then
	      best_nf := nf_e
	  done; failwith "toto"
	with
	|_ ->
	  !best_nf
      end
	
  let normal_form_old g =
    let pi = Refine.make (Flag.size g) in
    normal_form_constraint_old g (equitable_partition pi g)

  (* returns the moprhism p used insted of the normal form nf *)
  (* p satisfies [apply_morphism p g = (normal form of g)] *)
  let rec bad_normal_form_morphism_constraint g pi0 =
    let pi = equitable_partition pi0 g in
    let nfp e =
      let pi_e = individualize pi e in
      bad_normal_form_morphism_constraint g pi_e in
    let image p =
      Flag.apply_morphism p g in
    if is_discrete pi then
      perm_of_refine pi (*apply pi g*) (*Flag.apply_morphism (perm_of_refine pi) g*)
    else
      begin
	let s = choose_partition pi in
	let e = ref (Refine.first pi s) in
	let best_nfp = ref (nfp !e) in
	try
	  while e := Refine.next pi !e; !e <> -1 do
	    let nfp_e = nfp !e in
	    if image nfp_e > image !best_nfp then
	      best_nfp := nfp_e
	  done; failwith "toto"
	with
	|_ ->
	   !best_nfp
      end
	
  let bad_normal_form_typed_morphism sigma g =
    let pi = Refine.make_with_fixed_begin (Flag.size g) sigma in
    bad_normal_form_morphism_constraint g pi

                                        
  (* ** generating automorphisms with the same technic ** *)
      
  (* apply f to all permutations that is an automorphism of g *)
  let iter_on_automorphisms f g =
    let leaf = ref None in
    let treat_leaf h0 inv_p0 h p =
      if h0 = h then f (compose inv_p0 p) in
    let rec aux old_pi =
      let pi = equitable_partition old_pi g in
      if is_discrete pi then
	let p = perm_of_refine pi in
	let h = Flag.apply_morphism p g in
	match !leaf with
	| None ->
	  let inv_p = invert p in
	  leaf := Some (h, inv_p);
	  treat_leaf h inv_p h p
	| Some (h0, inv_p0) ->
	  treat_leaf h0 inv_p0 h p
      else
	let s = choose_partition pi in
	Refine.iter pi (fun e -> aux (individualize pi e)) s in
    aux (Refine.make (Flag.size g));;
  
  (* list all automorphisms *)
  let automorphisms g =
    let res = ref [] in
    let f p =
      res := p :: !res in
    iter_on_automorphisms f g;
    !res
      
  (* ** improved normal form algorithm ** *)

  (* largest common prefix of u and v (first common ancestor) *)
  let fca u v =
    let i = ref 0 in
    while u.(!i) = v.(!i) do incr i done;
    !i
      
  let normal_form_constraint g pi0 =
    let n = Flag.size g in
    let zeta = ref [] in
    let u = Array.make n [] in
    let v = Array.make n (-1) in
    let pi = Array.make n (Refine.copy pi0) in
    let k = ref 0 in
    let pop t i =
      let ti = t.(i) in
      t.(i) <- List.tl ti;
      List.hd ti in
    let treat_new new_g new_v =
      zeta := (new_g, new_v)::(!zeta)
    in
    let treat_leaf () =
      assert (is_discrete pi.(!k));
      let gv = apply pi.(!k) g in
      (try
	 let u = List.assoc gv !zeta in
	 k := fca u v
       with
       | Not_found -> treat_new gv (Array.copy v));
      decr k;
    in
    (* initialisation *)
    pi.(0) <- equitable_partition pi.(0) g;
    if is_discrete pi.(0)
    then treat_leaf ()
    else u.(0) <- choose_partition_list pi.(0);
    while !k >= 0 do
      if u.(!k) = []
      then decr k
      else
	begin
	  v.(!k + 1) <- pop u !k;
	  pi.(!k + 1) <- equitable_partition (individualize pi.(!k) v.(!k+1)) g;
	  incr k;
	  if is_discrete pi.(!k)
	  then treat_leaf ()
	  else u.(!k) <- choose_partition_list pi.(!k);
	end
    done;
    let (g, v) = list_max (fun (g1,_) (g2,_) -> compare g1 g2) !zeta in
    g
      
  let normal_form g =
    normal_form_constraint g (Refine.make (Flag.size g));; 
  
  let normal_form_typed sigma g =
    let pi = Refine.make_with_fixed_begin (Flag.size g) sigma in
    normal_form_constraint g pi

  (***** Partial untyping *****)
  (* given a (possibly not in normal form) flag [g] and a subset [set] of vertices
   of g,
  [unlabel_morphism g set] returns a permutation [pi] such that
  [apply pi g] is a proper representation (i.e. with the type in the begining) of
   the flag [g] with type [set] that is on normal form and such that its type is
   in normal form *)

  let size_set set =
    let res = ref 0 in
    Array.iter (fun b -> if b then incr res) set;
    !res

  let injection_of_indicator set =
    let n = size_set set in
    let i = ref 0 in
    let res = Array.make n (-1) in
    Array.iteri (fun j b -> if b then (res.(!i) <- j; incr i)) set;
    res
     
  (* returns a parmutation of {1,.., size of set} such that elements of
set are mapped to {1,2,3,...} *)
  let morphism_of_indicator set =
    let n = size_set set in
    let i = ref (-1)
    and j = ref (n - 1) in
    Array.map (fun b -> if b then (incr i; !i) else (incr j; !j)) set

  (* [induce_type sigma g] returns the type of g (of size sigma) as a flag *)
  let induce_type sigma g =
    Flag.induce (Array.init sigma (fun i -> i)) g

  (* [extend_permutation p n] returns the permutation on {1,..,[n]} which is
   equal to [p] in {1,...,Array.size p} and the identity on larger numbers *)
  let extend_permutation p n =
    let k = Array.length p in
    Array.init n (fun i -> if i < k then p.(i) else i)

  (* from a set as a bool array to int (with bits) *)
  let encode set =
    Array.fold_right (fun b x -> 2*x + if b then 1 else 0) set 0

  (* int (array of bits) to bool array *)
  let decode n x =
    let res = Array.make n false in
    let y = ref x in
    for i = 0 to n - 1 do
      res.(i) <- !y mod 2 = 1;
      y := !y/2
    done;
    res
      
  (* returns a morphism p : [n] -> [n] such that
      (a) p(set) = {1,..,k}
      (b) the subflag of p(g) induced by {1,..,k} is canonical
      (c) the flag p(g) with type {1,..,k} is canonical
   *) 
  let unlabel_morphism g set =
    let n = Flag.size g
    and k = size_set set in
    (* p1 satisfies (a) *)
    let p1 = morphism_of_indicator set in
    let g1 = Flag.apply_morphism p1 g in
    let gtype = induce_type k g1 in
    let qtype = bad_normal_form_typed_morphism 0 gtype in
    let q = extend_permutation qtype n in
    (* p2 satisfies (b) *)
    let p2 = Combinatoric.compose q p1 in
    let g2 = Flag.apply_morphism p2 g in
    let r = bad_normal_form_typed_morphism k g2 in
    (* p3 statisfies (c) *)
    let p3 = Combinatoric.compose r p2 in
    p3

  (* consider the canonical untype operation phi *)
  (* where id3 : size n, type k, typeid id2 *)
  (*       id2 : size k, no type *)
  (* and   id1 : size n, no type *)
  (* phi : ( m, n, id1 ) -> ( m, k, id2 ) *)

  let part_untype_tabulate k g3 g_array h_array =
    assert ( g_array <> [||] );
    assert ( h_array <> [||] );
    assert ( Flag.size g_array.(0) = Flag.size h_array.(0) );
    let m = Flag.size g_array.(0) in 
    let phi0 = bad_normal_form_typed_morphism k g3 in
    let psi = extend_permutation (invert phi0) m in
    let id_untyped g =
      let h = normal_form_typed k (Flag.apply_morphism psi g) in
      Common.array_search h_array h in
    Array.map id_untyped g_array
                                
  (*
  let canonical_untype sigma2 sigma1 g =
    assert (sigma2 >= sigma1);
    normal_form_typed sigma1 g
                      
  (* *)
  let canonical_untype_tabulate k g_array h_array =
    assert ( g_array <> [||] );
    assert ( h_array <> [||] );
(*    let m = Flag.size g_array.(0)
    and n = Flag.size h_array.(0) in*)
    let id_untyped g =
      let h = normal_form_typed k g in
      Common.array_search h_array h in
    Array.map id_untyped g_array
   *)
              
  let canonical_untype_tabulate k f3 g_array h_array =
    assert ( g_array <> [||] );
    assert ( h_array <> [||] );
    assert ( Flag.size g_array.(0) = Flag.size h_array.(0) );
    let m = Flag.size g_array.(0) in 
    let phi0 = bad_normal_form_typed_morphism k f3 in
    let psi = extend_permutation (invert phi0) m in
    let id_untyped g =
      let h = normal_form_typed k (Flag.apply_morphism psi g) in
      Common.array_search h_array h in
    Array.map id_untyped g_array
                           
  (************ Razborov functions **************)
      
  (* ** combinatorics ** *)

  (* compute the increasing injection from [Array.length p] to [n] *)
  (* following p in lexicographic order *)
  let next n p =
    let k = Array.length p in
    let i = ref (k - 1) in
    while !i >= 1 && p.(!i) = n - k + !i do
      decr i
    done;
    p.(!i) <- p.(!i) + 1;
    for j = !i + 1 to k - 1 do
      p.(j) <- p.(j-1) + 1
    done;;
  
  (* apply f to the (k-sigma choose n) subsets of [n] of size k containing [sigma] *)
  let iter_on_subsets f sigma k n =
    let map = Array.init k (fun i -> i) in
    while
      if sigma = 0 then map.(0) <= n - k else map.(sigma-1) = sigma-1
    do
      f map;
      next n map
    done;;
  
  (* apply f on the n! permutations of p that only move p.(0),...,p.(n-1) *)
  (* p is modified in place but is returned unchanged *)
  let rec iter_on_permutations f n p =
    if n <= 1 then f p
    else
      for i = 0 to n-1 do
	swap p i (n-1);
	iter_on_permutations f (n-1) p;
      swap p i (n-1);
      done
	
  (* apply f to the ((n-k+1)*..(n-1)*n) injections from [k] to [n] *)
  let iter_on_injections f k n =
    iter_on_subsets (iter_on_permutations f k) 0 k n

  (* select is an increasing injection from [k] to [n] fixing [sigma],
     it represent a subset of [n] containing [sigma] (which is precisely its image).
     complementary n sigma select returns the increasing injection
     from [n - k + sigma] to [n] fixing [sigma] representing the compementary of
     the image of select union [sigma] *)
  let complementary n sigma select =
    let k = Array.length select in
    let res = Array.make (n - k + sigma) 0 in
    for i = 0 to sigma-1 do
      res.(i) <- i
    done;
    let j = ref sigma
    and l = ref sigma in
    for i = sigma to n -1 do
      if !j < k && i = select.(!j)
      then incr j
      else (res.(!l) <- i; incr l)
    done;
    res
      
  (* ** density ** *)
      
  (* counts the number of induced subflags of g isomorphic to h *)
  let count_subflags sigma h g =
    assert (h = normal_form_typed sigma h);
    let n = Flag.size g
    and k = Flag.size h in
    if k > n then 0
    else
      let count = ref 0 in
      let test_map map =
	let h' = Flag.induce map g in (*WARNING: here we assume that Flag.map_induce does not scramble the type *)
	if h = normal_form_typed sigma h' then incr count in
      iter_on_subsets test_map sigma k n;
      !count
	
  (* probability of a subflag *)
  let p sigma h g =
    let k = Flag.size h
    and n = Flag.size g in
    assert (k <= n);
    Rational.make (count_subflags sigma h g) (binomial (k - sigma) (n - sigma))
      
  (* density of two disjoint subflags ( p(h, h'; g) ) *)
  let p2 sigma h h' g =
    assert (h = normal_form_typed sigma h);
    assert (h' = normal_form_typed sigma h');
    let k = Flag.size h
    and k' = Flag.size h'
    and n = Flag.size g in
    assert (n >= k + k' - sigma);
    let pi_k = Refine.make_with_fixed_begin k sigma in
    let count = ref 0 in
    let test_map map =
      let f = Flag.induce map g in
      if h = normal_form_constraint f pi_k then
	let f' = Flag.induce (complementary n sigma map) g in
	let c = count_subflags sigma h' f' in
      (*pperm map;
	Printf.printf "count += %d\n" i; flush stdout;
	Flag.draw_list [g;f;f'];*)
	count := !count +  c
  in
    iter_on_subsets test_map sigma k n;
  let total =
    (binomial (k - sigma) (n - sigma)) * (binomial (k' - sigma) (n - k)) in
  Rational.make !count total
      
  (* optimized versions *)
  (* return an array indiced on g of matrices indiced on h1 times h2 *)
  (* Order of the coordinates : g h1 h2 *)
  let p2_tabulate sigma h1_array h2_array g_array =
    let k1 = Flag.size h1_array.(0)
    and k2 = Flag.size h2_array.(0)
    and n = Flag.size g_array.(0) in
    assert (n = k1 + k2 - sigma);
    let l1 = Array.length h1_array
    and l2 = Array.length h2_array
    and l = Array.length g_array in
    let nf = normal_form_typed sigma in
    let init i = (* computes the slice i *)
      let r = Array.make_matrix l1 l2 0 in
      let use_map map =
	let co_map = complementary n sigma map in
	let h1 = nf (Flag.induce map g_array.(i))
	and h2 = nf (Flag.induce co_map g_array.(i)) in
	let i1 = array_search h1_array h1
	and i2 = array_search h2_array h2 in
	r.(i1).(i2) <- r.(i1).(i2) + 1
      in
      iter_on_subsets use_map sigma k1 n;
      Sparse.of_dense r
    in
    Array.init l init

  (* TO BE MOVED *)
  let p_denom sigma k n  = binomial (k - sigma) (n - sigma)

  (*
    let p_tabulate_old sigma h_array g_array =
    let k = Flag.size h_array.(0)
    and n = Flag.size g_array.(0) in
    assert (sigma <= k && k <= n);
    let l = Array.length h_array
    and m = Array.length g_array in
    
    let trick rat = (* TO BE REPLACED *)
      (Rational.nom rat) * (p_denom sigma k n) / (Rational.denom rat) in
    let r = Array.init m
      (fun i -> Array.init l
	(fun j ->
	  trick (p sigma h_array.(j) g_array.(i)))) in
    Sparse.of_dense r *)

  (* p.(i).(j) is the razborov function p(H_j; G_i) *)
  (* indices are ordered so that P X is the projection of X *)
  let p_tabulate sigma h_array g_array =
    let k = Flag.size h_array.(0)
    and n = Flag.size g_array.(0) in
    assert (sigma <= k && k <= n); 
    let l = Array.length h_array
    and m = Array.length g_array in
    let res = Sparse.make_dynmat m l in
    let spa = Sparse.makeSPA l in
    (* for each line *)
    for i = 0 to m - 1 do
      let g = g_array.(i) in
      let use_map map =
	let h = normal_form_typed sigma (Flag.induce map g) in
	let j = array_search h_array h in
	Sparse.scatterSPA spa 1 j in
      iter_on_subsets use_map sigma k n;
      Sparse.gatherSPA spa res
    done;
    Sparse.dynmat_to_sparse res
    
  let p2_denom sigma k1 n = binomial (k1 - sigma) (n - sigma)

  let q_denom sigma n = (binomial sigma n) * (fact sigma)

  let part_q_denom sigma1 sigma2 n =
    assert ( sigma1 >= sigma2 );
    q_denom ( sigma1 - sigma2 ) ( n - sigma2 )
                                               
  (* unlabeling factor *)
  let q_nom k g =
    let acc = ref [] in
    let record a =
      acc := (Array.init k (Array.get a)) :: !acc in
    iter_on_automorphisms record g;
    let valid_types = remove_duplicated !acc in
    List.length valid_types
    
  let q k g =
    let nom = q_nom k g
    and denom = q_denom k (Flag.size g) in
    Rational.make nom denom

  (* unlabeling factor for partial unlabeling *)
      
  (* partial unlabeling *)
      
  (* g is a flag and p an injection of the type *)
  let partial_unlabel g p =
    let sigma = Array.length p in
    let q = permutation_of_injection p (Flag.size g) in
    let g' = Flag.apply_morphism q g in
    normal_form_typed sigma g'
      
  (********* Spanning flags **********)
      
  module FlagSet = Set.Make (struct type t = Flag.t let compare = compare end)
    
  (* ** untyped flags ** *)
    
  (* Basic method *)
  let span_flags n =
    let raw_flags = Flag.span n in
    List.rev (Common.remove_duplicated (List.map normal_form raw_flags))
      
  (* Iterative method *)
  let span_flags_next l =
    let set = ref FlagSet.empty in
    let record flag = set := FlagSet.add (normal_form flag) !set in
    let treat smallflag =
      List.iter record (Flag.superflags smallflag) in
    List.iter treat l;
    FlagSet.elements !set
  
  let span_flags_iterative n =
    let rec aux l = function
      | 1 -> l
      | k -> aux (span_flags_next l) (k-1) in
    aux (span_flags 1) n;;
  
  (* ** typed flags ** *)
  
  (* iter_span_typed f h g applies f on all flag isomorphic to g with type h *)
  let iter_span_typed f h g =
    let n = Flag.size g
    and k = Flag.size h in
    let test_map map =
      let h' = Flag.induce map g in
      if h' = h then
	let p = invert (permutation_of_injection map n) in
	let g' = normal_form_typed k (Flag.apply_morphism p g) in
	f g' in
    iter_on_injections test_map k n;;
  
  (* make a list of all flags isomorphic to g with type h *)
  let list_span_typed h g =
    let res = ref [] in
    iter_span_typed (fun g' -> res := g' :: !res) h g;
    !res
      
  (* span_all_typed h [g1,..,gn] makes a list of all flags with type h isomorphic to one gi *)
  let span_all_typed h untyped =
    let set = ref FlagSet.empty in
    let record flag = set := FlagSet.add flag !set in
    List.iter (iter_span_typed record h) untyped;
  FlagSet.elements !set

end
