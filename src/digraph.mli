(** Flag set of directed graphs.

This module is compatible with the Flag.S signature *)
type t = { n : int; e : (int * int) array }
val name : string
val size : t -> int
val in_degrees : t -> int array
val out_degrees : t -> int array
val in_neibrs : t -> int -> int array
val out_neibrs : t -> int -> int array
val iso_invariant : t -> int -> int list array
val invariant_size : int
val make : int -> (int * int) array -> t
val induce : int array -> t -> t
val apply_morphism : int array -> t -> t
val draw : ?root:int -> t -> int -> int -> int -> unit
val print : t -> string
val span : int -> t list
val superflags : t -> t list
