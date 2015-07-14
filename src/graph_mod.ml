(* type provisoire *)
(* Graph.t = int * ( (int*int) list )*)
type t = Graph.t

let size g = (Graph.size g) - 2

let shift s =
  let n = Array.length s in
  let new_s = Array.make (n + 2) (-1) in
  new_s.(0) <- 0;
  new_s.(1) <- 1;
  for i = 0 to n-1 do
    new_s.(i+2) <- s.(i) + 2
  done;
  new_s

let unshift s =
  let n = Array.length s in
  assert (n > 2);
  Array.init (n - 2) (fun i -> s.(i+2))

let iso_invariant g s =
  let l =
    Array.map
      (List.filter (fun x -> x >= 2)) (Graph.iso_invariant g (s+2)) in
  Array.map (List.map (fun x -> x-2)) l

let invariant_size = Graph.invariant_size

let print = Graph.print



let apply_morphism s g =
  Graph.apply_morphism (shift s) g

let induce m g =
  Graph.induce (shift m) g

let draw = Graph.draw

let filter = List.filter (fun new_g -> List.mem (1,0) (Graph.edges new_g))

let superflags g =
  filter (Graph.superflags g)

let span n =
  filter (Graph.span (n+2))

let name = "mod_graphs"
