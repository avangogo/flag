(** Flag set of directed graphs without triangle.

This module only differs from Digraph by its functions for generating flags *)

include Digraph

let name = "trianglefree"

exception Triangle_found
exception Triangle_free

let rooted_triangle_free v g =
  try
    let in_v = in_neibrs g v
    and out_v = out_neibrs g v in
    let j = ref 0 in
    let m = Array.length g.e in
    for o = 0 to (Array.length out_v) - 1 do
      for i = 0 to (Array.length in_v) - 1 do
	let edge = (out_v.(o), in_v.(i)) in
	while !j < m && g.e.(!j) < edge do
	  incr j;
	done;
	if !j = m then raise Triangle_free;
	if  g.e.(!j) = edge then raise Triangle_found
      (* otherwise g.e.(!j) > (o, i) *)
      done
    done;
    true
  with
  | Triangle_free -> true
  | Triangle_found -> false

let triangle = make 3 [| (0, 1); (1, 2); (2, 0) |]

let naive_triangle_free g =
  let res = ref true in
  for u = 0 to g.n-1 do
    for v = 0 to g.n-1 do
      for w = 0 to g.n-1 do
	if u <> v && v <> w && w <> u then
	  let h = induce [| u; v; w |] g in
	  if h = triangle then res := false
      done
    done
  done;
  !res

let span n =
  List.filter naive_triangle_free (Digraph.span n)

let superflags g =
  List.filter (rooted_triangle_free g.n) (Digraph.superflags g)
