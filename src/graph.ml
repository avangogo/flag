let draw_label = false

(* type provisoire *)
type t = int * ( (int*int) list )

let normalize_edge (u, v) =
  if u >= v then (u, v) else (v,u)

let normalize (n, e) =
  let new_e = List.sort compare (List.map normalize_edge e) in
  (n, new_e)

let make_unsafe n edges =
  normalize (n, edges)

let make n edge =
  let check_edge (i, j) =
    if i < 0 || j < 0 || i >= n || j >= n then failwith
      (Printf.sprintf
	 "Graphs.make : edge (%d, %d) out of range (size is %d)" i j n) in
  let rec aux = function
    | ((i, j) as a)::b::q ->
      if a=b then failwith
	(Printf.sprintf "Graph.make : edge (%d, %d) duplicated" i j)
      else aux (b::q)
    | _ -> () in
  List.iter check_edge edge;
  aux (List.sort compare (List.map normalize_edge edge));
  make_unsafe n edge

let edges (_, e) = e
let size (n, _) = n

let neibrs g v =
  (List.map fst (List.filter (fun (_, u) -> v = u) (edges g))) @
    (List.map snd (List.filter (fun (u, _) -> v = u) (edges g)))

let iso_invariant g v =
  [| neibrs g v |]

let invariant_size = 1

let print g =
  (* let g = normalize g0 in *)
  Printf.sprintf "Graph of size %d\nEdges: %s\n"
    (size g)
    (String.concat " " (List.map (fun (u,v) -> Printf.sprintf "(%d, %d)" u v) (edges g)))

let apply_morphism s (n, e) =
  let new_edges = List.map (fun (i, j) -> (s.(i), s.(j))) e in
  normalize (n, new_edges) (* perte de vitesse? *)

let induce m (n, e) =
  let k = Array.length m in
  let inv_m = Combinatoric.pseudo_invert n m in
  let rec aux acc = function
    | (i,j)::q ->
      let new_i = inv_m.(i)
      and new_j = inv_m.(j) in
      if new_i = -1 || new_j = -1
      then aux acc q
      else aux ((new_i, new_j)::acc) q
    | [] -> acc in
  normalize (k, aux [] e)

let randomize (n, e) =
  let (n', e') = apply_morphism (Common.random_perm n) (n, e) in
  (n', Common.randomize_list e')
  
let gnp n p =
  let e = ref [] in
  for i = 0 to n-1 do
    for j = 0 to i-1 do
      if Random.float 1. < p then e := (i, j)::!e
    done
  done;
  (n, !e)

let draw ?root:(root=0) g i j c =
  let polar rho theta = (rho *. (cos theta), rho *. (sin theta)) in
  let n = float_of_int (size g) in
  let pos v =
    let rho = (float_of_int c)/.3. in
    let x, y = polar rho ((3.14 *. 2. *. (float_of_int v)) /. n) in
    (i + c/2 + (int_of_float x), j + c/2 + (int_of_float y)) in
  let draw_edge (i, j) =
    let xi, yi = pos i
    and xj, yj = pos j in
    Graphics.moveto xi yi;
    Graphics.lineto xj yj
  in
  List.iter draw_edge (edges g);
  for i = 0 to (size g) -1 do
    if i < root then Graphics.set_color Graphics.red;
    let xi, yi = pos i in
    Graphics.fill_circle xi yi (max 2 (c/20));
    if draw_label
    then begin
      Graphics.moveto xi yi;
      Graphics.draw_string (Printf.sprintf "  %d" i)	
      end;
    Graphics.set_color Graphics.black
  done

(* generating graphs *)
let possible_edges n =
  let rec aux acc = function
    | -1 -> acc
    | k  -> aux ((k, n)::acc) (k - 1) in
  aux [] (n - 1)

let superflags g =
  let n = size g in
  let new_edges_list = Combinatoric.all_subsets (possible_edges n) in
  let edges = edges g in
  let assembly new_edges =
    make_unsafe (n+1) (edges @ new_edges) in
  List.rev_map assembly new_edges_list

(* make a list of all graphs of size n *)
let span n =
  let all_edges =
    let res = ref [] in
    for i = 0 to n - 1 do
      for j = 0 to i - 1 do
	res := (i, j) :: !res
      done
    done;
    !res in
  let edges = Combinatoric.all_subsets all_edges in
  let graphs = List.rev_map (make_unsafe n) edges in
  graphs

let name = "graphs"
