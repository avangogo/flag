(** Flag set of graphs.

This module is compatible with the Flag.S signature *)
type t
val size : t -> int
val iso_invariant : t -> int -> int list array
val invariant_size : int

val induce : int array -> t -> t
val apply_morphism : int array -> t -> t

val draw : ?root:int -> t -> int -> int -> int -> unit
val print : t -> string

(* generating flags *)
val superflags : t -> t list
val span : int -> t list

(* test functions *)
val randomize : t -> t

(* graph specific *)
val make : int -> (int * int) list -> t
val edges : t -> (int * int) list
val gnp : int -> float -> t
val name : string
val normalize_edge : (int * int) -> (int * int)
val neibrs : t -> int -> int list
