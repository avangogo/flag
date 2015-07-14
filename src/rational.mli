(** Rational numbers. *)

type t
val nom : t -> int
val denom : t -> int
val zero : t
val make : int -> int -> t
val int : int -> t
val rat : t -> t
val mul : t -> t -> t
val div : t -> t -> t
val add : t -> t -> t
val sub : t -> t -> t
val minus : t -> t
val compare : t -> t -> int
val leq : t -> t -> bool
val to_float : t -> float
val to_int : t -> int
val print : t -> string
