open Common

include Common

type t = { n : int; e : (int * int) array }

let name = "digraph"

let size g = g.n

let in_neibrs g v =
  Array.map fst (array_filter (fun (_, u) -> v == u) g.e)

let out_neibrs g v =
  Array.map snd (array_filter (fun (u, _) -> v == u) g.e)

let iso_invariant g v =
  Array.map Array.to_list
    [| in_neibrs g v; out_neibrs g v |]

let invariant_size = 2

let sort_e e =
  Array.sort compare e

let make n e =
  let new_e = Array.copy e in
  sort_e new_e;
  { n = n ; e = new_e }

let induce f g =
  let new_n = Array.length f in
  let inv_f = Combinatoric.pseudo_invert g.n f in
  let aux_e = ref [] in
  let e = g.e in
  for i = 0 to (Array.length e) - 1 do
    let (u, v) = e.(i)  in
    let new_u = inv_f.(u)
    and new_v = inv_f.(v) in
    if new_u != -1 && new_v != -1
    then aux_e := (new_u, new_v) :: !aux_e
  done;
  let new_e = Array.of_list !aux_e in
  sort_e new_e;
  { n = new_n ; e = new_e }

let apply_morphism_edge f (u, v) =
  (f.(u),  f.(v))

let apply_morphism f g =
  let new_e = Array.map (apply_morphism_edge f) g.e in
  sort_e new_e;
  { n = g.n ; e = new_e}

(* generating digraphs *)
let id_edge n (u, v) =
  let x = ((n - 1) * u) + v in
  if v < u then x else x - 1

let ith_edge n i =
  let u = i / (n-1)
  and v = i mod (n-1) in
  ( u, if v < u then v else v + 1 )

let _ = assert (List.for_all
		  (fun i -> id_edge 6 (ith_edge 6 i) = i)
		  [0;1;2;7;8;10;13;14;15])

let rev_edge n i =
  let (u, v) = ith_edge n i in
  id_edge n (v, u)

let possible_edges n =
  Array.init ((n-1)*n) (ith_edge n)
  
let choose_edges f all sister =
  assert (let x = Array.copy all in
	  Array.sort compare x;
	  x = all);
  let max = Array.length all in
  let used_edge = Array.make max false in
  let t = Array.make (max/2) (-1, -1) in
  let rec explore i (* index on all *) m (* index on t *) =
    if i = max then
      let res = Array.sub t 0 m in
      assert (let x = Array.copy res in
	      Array.sort compare x;
	      x = res);
      f res
    else
      if used_edge.(i)
      then explore (i+1) m
      else
	begin
	  t.(m) <- all.(i);
	  used_edge.(sister.(i)) <- true;
	  explore (i+1) (m+1); (* add the edge *)
	  used_edge.(sister.(i)) <- false;
	  explore (i+1) m (* skip the edge *)
	end in
  explore 0 0

let span n =
  let all = possible_edges n in
  let max = Array.length all in
  let sister = Array.init max (rev_edge n) in
  let res = ref [] in
  let use t =
    res := { n = n ; e = t } :: !res in
  choose_edges use all sister;
  !res

let superflags g =
  let n = g.n in
  let max = n * 2 in
  let all = Array.init max (fun i -> if i < n then (i, n) else (n, (i-n)))
  and sister = Array.init max (fun i -> (i + n) mod max ) in
  let res = ref [] in
  let use t =
    res := { n = n+1 ; e = array_merge g.e t } :: !res in
  choose_edges use all sister;
  !res

(* *** Print and draw functions *** *)

let print g =
  let print_edge (u, v) =
    Printf.sprintf "(%d, %d)" u v in
  Printf.sprintf "Digraph of size %d\nEdges: %s\n"
    g.n
    (String.concat " " (Array.to_list (Array.map print_edge g.e)))

let draw_label = false

let draw ?root:(root=0) g i j c =
  let polar rho theta = (rho *. (cos theta), rho *. (sin theta)) in
  let orth (x, y) = (-y, x) in
  let normalize l (x, y) =
    let norm = sqrt (float_of_int (x*x) +. float_of_int (y*y)) in
    let ratio = (float_of_int l) /. norm in
    let aux a = int_of_float ((float_of_int a) *. ratio) in
    (aux x, aux y)
  in
  let n = float_of_int (size g) in
  let node_size = (max 2 (c/20)) in
  let pos v =
    let rho = (float_of_int c)/.3. in
    let x, y = polar rho ((3.14 *. 2. *. (float_of_int v)) /. n) in
    (i + c/2 + (int_of_float x), j + c/2 + (int_of_float y)) in
  let draw_edge (i, j) =
    let xi, yi = pos i
    and xj, yj = pos j in
    let xd, yd = xj - xi, yj - yi in
    let xm, ym = normalize node_size (xd, yd) in
    Graphics.moveto xi yi;
    Graphics.lineto xj yj;
    let xo, yo = orth (xd/10, yd/10) in
    Graphics.fill_poly [|(xj - xm, yj -ym);
			 (xj - xm - xd/4 - xo, yj - ym - yd/4 - yo);
			 (xj - xm - xd/5, yj - ym - yd/5);
			 (xj - xm - xd/4 + xo, yj - ym - yd/4 + yo)|]
  in
  Array.iter draw_edge g.e;
  for i = 0 to g.n -1 do
    if (i < root) then Graphics.set_color Graphics.red;
    let xi, yi = pos i in
    Graphics.fill_circle xi yi node_size;
    if draw_label
    then begin
	Graphics.moveto xi yi;
	Graphics.draw_string (Printf.sprintf "  %d" i)	
      end;
    Graphics.set_color Graphics.black
  done
