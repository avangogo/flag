(** Flag signature. *)

module type S =
sig
  type t
  val size : t -> int
  val iso_invariant : t -> int -> int list array
  val invariant_size : int
  val induce : int array -> t -> t
  val apply_morphism : int array -> t -> t
  val draw : ?root:int -> t -> Graphic.drawable
  val print : t -> string
  val name : string
  (* generating flags *)
  val superflags : t -> t list
  val span : int -> t list
end
