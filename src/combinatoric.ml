(** Basic combinatorial functions. *)

(** {8 Counting. } *)

(** Factorial. *)
(* we should have [n < 24] to avoid int overflow *)
let fact n =
  let rec f acc = function
    | 0 -> acc
    | n -> f (acc * n) (n - 1) in
  f 1 n

(**/**)
(* returns acc*k*(k+1)*..*(n-1)*n *)
let rec prod acc k n =
  if k > n then acc
  else prod (acc * k) (k+1) n
(**/**)

(** [binomial k n] returns [n] choose [k]. *)
let binomial k n =
  if k < 0 || k > n then 0
  else (prod 1 (n-k+1) n)/(prod 1 1 k)

(** {8 Permuations. } *)
(** Mappings from [\{0,...,k-1\}] to [\{0,...,n-1\}] to are represented by an array of integers *)


(** Inverts an injection. Value without antecedant will have [-1]. *)
let pseudo_invert n t =
  let k = Array.length t in
  let r = Array.make n (-1) in
  for i = 0 to k-1 do
    r.(t.(i)) <- i
  done;
  r

(** Inverts a bijection. *)
let invert t =
  pseudo_invert (Array.length t) t

(** computes antecedents of [\{0, .., n-1\}] *)
let pre_image n t =
  let res = Array.make n [] in
  for i = 0 to (Array.length t) - 1 do
    res.(t.(i)) <- i :: res.(t.(i))
  done;
  Array.map Array.of_list res
    
(** Assuming [t] is an injection from [\{0, .., k-1\}] to [\{0, .., n-1\}],
[permutation_of_injection t n] returns the unique bijection of [\{0, .., n-1\}]
that is equal to [t] on [\{0,...,k-1\}] and is increasing on [\{k, .., n-1\}]. *)
let permutation_of_injection map n =
  let k = Array.length map in
  let res = Array.make n (-1) in
  let used = Array.make n (false) in
  for i = 0 to k-1 do
    used.(map.(i)) <- true
  done;
  let j = ref 0 in
  Array.blit map 0 res 0 k;
  for i = k to n-1 do
    while used.(!j) do incr j done;
    res.(i) <- !j;
    incr j;
  done;
  res
    
(** [compose p q] returns the map [\[| p.(q.(0)); ...; p.(q.(n-1)) |\]]. *)
let compose p q =
  assert (Array.length p = Array.length q);
  let n = Array.length p in
  Array.init n (fun i -> p.(q.(i)))

(** {8 Others. } *)

(** [all_subsets l] returns the list of all 2^(length l) sublists of [l] *)
let rec all_subsets = function
  | t::q ->
    let a = all_subsets q in
    List.rev_append (List.rev_map (fun l -> t::l) a) a
  | [] -> [[]]
