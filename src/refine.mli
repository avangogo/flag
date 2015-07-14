(** Partition refinement structure.

This module implements partition refinement structures that are modified in place as described in http://arxiv.org/pdf/0802.2826v1.pdf.
*)

type t
(** The type of partitions of [\{0,..,n-1\}]. *)
type elt = int
(** The elements of the partition are of type {!int}. Begin at [0]. *)
type set = int
(** The type of the sets of a partition. They are [int] between [0] and [n-1] *)
val make : int -> t
(** [make n] returns a new partition of [\{0,..,n-1\}], with only one set containing all elements. *)
val sets : t -> int
val size : t -> set -> int
val total_size : t -> int
val set : t -> elt -> set
val first : t -> set -> elt
val next : t -> elt -> elt
val mark : t -> elt -> unit
val split : t -> set -> set
val no_marks : t -> set -> bool
val iter : t -> (elt -> unit) -> set -> unit
val copy : t -> t

val cut : t -> (set -> unit) -> elt list -> unit
(** Combination of the preceding functions that cuts among a set :
[cut p f l] separates the elements of [l] from the others in all partitions of [p], then applies [f] on each newly created subset *)

val make_with_fixed_begin : int -> int -> t
(** [make_with_fixed_begin n k] returns a new partition with [n] elements and [k+1] partition sets [\{0\},\{1\},...,\{k-1\},\{k,...,n-1\}] where the partition [i]-th is [\{i\}] for [0 <= i <= k-1]. This last property will be conserve by further operations on p. *)
