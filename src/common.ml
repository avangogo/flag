(** General functions.

General functions that could be in the standard library. *)


let swap t i j =
  let aux = t.(i) in
  t.(i) <- t.(j);
  t.(j) <- aux;;

(** [list_iteri f \[a0; ...; an-1\]] is equivalent to [f 0 a0; f 1 a1; ... ; f n-1 an-1; ()]. *)
let list_iteri f (*int -> 'a -> unit*) =
  let rec aux i  = function
    |t::q -> (f i t); aux (i+1) q
    |[]   -> () in
    aux 0
(* Implemented in the standard libratry of OCaml 4.00 *)

(** [list_init n f] returns the list [\[f 0; ... ; f (n-1)\]]. *)
let list_init n f =
  let rec aux acc = function
    | 0 -> acc
    | i -> aux ((f (i-1)) :: acc) (i-1)
  in
  aux [] n

let matrix_array_of_array_matrix mat =
  let m1 = Array.length mat in
  let m2 = Array.length mat.(0) in
  let n = Array.length mat.(0).(0) in
    (* mat is a m*m matrices of vectors of size n *)
  let res = Array.init n (fun _ -> Array.make_matrix m1 m2 mat.(0).(0).(0)) in
  for i = 0 to n-1 do
    for j = 0 to m1-1 do
      for k = 0 to m2-1 do
	res.(i).(j).(k) <- mat.(j).(k).(i)
      done
    done
  done;
  res

(** Usage : [apply_matrix zero add mul m v] where zero, add, mul are the corresponding values and operations in the considered field *)
let apply_matrix (zero : 'a) (add : 'a->'a ->'a) (mul : 'a->'a->'a) a v =
  let n = Array.length a
  and m = Array.length a.(0) in
  let res = Array.make n zero in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
	res.(i) <- add res.(i) (mul a.(i).(j) v.(j))
    done
  done;
  res

(** [apply_matrix zero add mul m1 m2] as for {!apply_matrix}. *)
let multiply_matrix (zero : 'a) (add : 'a -> 'a -> 'a) (mul : 'a -> 'a -> 'a) a b =
  let n = Array.length a
  and l = Array.length b
  and m = Array.length b.(0) in
  let res = Array.make_matrix n m zero in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      for k = 0 to l-1 do
	res.(i).(j) <- add res.(i).(j) (mul a.(i).(k) b.(k).(j))
      done
    done
  done;
  res

let bilinear_form zero add mul a m b =
  let apply = apply_matrix zero add mul
  and multiply = multiply_matrix zero add mul in
  (apply (multiply [|a|] m) b).(0)

let matrix_map f m = Array.map (Array.map f) m

let option_map f = function
  | Some x -> Some (f x)
  | None -> None

let option_map2 f o1 o2 =
  match o1, o2 with
  | Some x1, Some x2 -> Some (f x1 x2)
  | None, _ -> None
  | _, None -> None

let option_default x = function
  | Some y -> y
  | None -> x

let array_map2 f v1 v2 =
  let n = Array.length v1 in
  assert (Array.length v2 = n);
  Array.init n (fun i -> f v1.(i) v2.(i))

(* One could avoid the Array.to_list application, I don't see more interresting optimization *)
let array_filter f t =
  Array.of_list (List.filter f (Array.to_list t))

let array_for_all f t =
  let n = Array.length t in
  let rec aux i =
    (i == n) || (f t.(i) && aux (i + 1)) in
  aux 0

let array_merge t1 t2 =
  let n1 = Array.length t1
  and n2 = Array.length t2 in
  if n1 = 0 then t2
  else
    let m = n1 + n2 in 
    let res = Array.make m t1.(0) in
    let rec aux i1 i2 j =
      if i1 = n1 then Array.blit t2 i2 res j (m - j)
      else if i2 = n2 then Array.blit t1 i1 res j (m - j)
      else
	let a1 = t1.(i1) and a2 = t2.(i2) in
	if a1 <= a2 then
	  ( res.(j) <- a1;
	    aux (i1 + 1) i2 (j + 1) )
	else
	  ( res.(j) <- a2;
	    aux i1 (i2 + 1) (j + 1) )
    in
    aux 0 0 0;
    res

(** Assuming [ t ] is sorted,
    [ array_search t a ] returns an indice [ i ] such that [ t.(i) = a ] if some exists
    and raises [ Not_foud ] exception otherwise. *)
let array_search t a =
  let rec search min max =
    if min = max then raise Not_found
    else
      let mid = (min + max)/2 in (* we have min <= mid < max *)
      if a < t.(mid) then search min mid
      else if a > t.(mid) then search (mid + 1) max
      else (* a=t.(mid) *) mid in
  search 0 (Array.length t)

let array_mem a t =
  try
    let _ = array_search t a in
    true
  with
  | Not_found -> false

(** Multisets are represented by list of (int, element) sorted by element. *)
let makeMultiset l =
  List.map (fun x -> (1, x)) (List.sort compare l)

let rec mergeMultiset m1 m2 =
  match m1 with
  | [] -> m2
  | (n1, v1)::q1 ->
    match m2 with
    | [] -> m1
    | (n2, v2)::q2 ->
      if v1 < v2 then (n1, v1)::(mergeMultiset q1 m2)
      else if v1 = v2 then (n1+n2, v1)::(mergeMultiset q1 q2)
      else (* v1 > v2 *) (n2, v2)::(mergeMultiset m1 q2)

let makeAssoc l =
  let rec aux res acc_el = function
    | (k, a)::((l, _)::_ as q) ->
      if k = l then aux res (a::acc_el) q  
      else aux ((k, a::acc_el)::res) [] q
    | [k, a] -> (k, a::acc_el)::res
    | [] -> [] in
  let sorted_l = List.sort (fun (a, _) (b, _) -> compare a b) l in
  aux [] [] sorted_l;;

(** [remove_duplicated l] returns a list containing each element of l exactly once. The outputed list is sorted in decreasing order. *)
let remove_duplicated l =
  let rec aux acc = function
    | a :: b :: q ->
      if a = b then aux acc (a :: q)
      else aux (a :: acc) (b :: q)
    | [ a ] -> a :: acc 
    | [] -> acc in
  aux [] (List.sort compare l);;

let list_nth_first n =
  let rec aux i acc = function
    | t::q ->
      if i = 0 then acc
      else aux (i - 1) (t::acc) q
    | [] -> acc in
  aux n []

let list_max compare = function
  | [] -> failwith "list_max"
  | t::q -> List.fold_left
    (fun a b -> if compare a b >= 0 then a else b) t q;;

(** {8 Random functions. } *)

Random.self_init ();;

(** Scramble the input array. *)
let randomize_array t =
  let n = Array.length t in
  for i = n-1 downto 1 do
    swap t (Random.int (i+1)) i
  done

(** Pick a permutation uniformly at random. *)
let random_perm n =
  let s = Array.init n (fun i -> i) in
  randomize_array s;
  s

(** Return a scrambled version of the input list. *)
let randomize_list l =
  let t = Array.of_list l in
  randomize_array t;
  Array.to_list t
