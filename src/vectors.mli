(** Quantum flags. *)
open Storage

(** A [('a, 'b) vector] object [v] represents "sum_i v.vect.(i) F_i"
   where F_i is the i-th flag of the basis [b.basis] *)
type ('a, 'b) vector = {
  name : string option;
  expr : Prettyprinting.expr;
  basis : 'b basis_id;
  vect : 'a array;
}

val map : ('a -> 'b) -> ('a, 'c) vector -> ('b, 'c) vector

module Vect :
  functor (F : Field.S) ->
    functor (Flag : Flag.S) ->
sig
  type vect = (F.t, Flag.t) vector
  val get_q : Flag.t basis_id -> F.t array
  val get_untype : Flag.t basis_id -> int array
  val get_p : Flag.t basis_id -> Flag.t basis_id -> F.t array array
  val get_p2 :
    Flag.t basis_id ->
    Flag.t basis_id ->
    Flag.t basis_id -> F.t array array array
  val get_size : Flag.t basis_id -> int

  (** Apply the unrooting operation. *)
  val untype : vect -> vect

  (** Projects the vector on a basis of more flag. *)
  val expand : Flag.t basis_id -> vect -> vect

  (** Multiplication of the flag algebra. *)
  val multiply : vect -> vect -> vect
    
  (** Scalar multiplication of the vector space structure.
  The optional name is the name of the scalar.*)
  val scalar_mul : ?name:string -> F.t -> vect -> vect

  (** Vector addition. *)
  val add : vect -> vect -> vect

  (** Vector substraction. *)
  val sub : vect -> vect -> vect

  (** Multiplication by -1. *)
  val opposite : vect -> vect

  (** [flag_of_id b i] creates the vector representing
      the [i]-th flag of the basis [b] (It is a vector with 
      a 1 coefficient and 0's elsewhere). *)
  val flag_of_id : ?name:string -> Flag.t basis_id -> int -> vect

  (** Same as {!flag_of_id} except that the flag is given explicitly
      (for instance as a graph) instead of beeing identified by its
      number. *)
  val flag : ?name:string -> Flag.t basis_id -> Flag.t -> vect

  (** The zero vector. *)
  val empty : Flag.t basis_id -> vect

  (** Creates the sum of all flags of the given basis
      (i.e. the vector with only 1's).
      This is also the "one" element for the algebra structure. *)
  val one : Flag.t basis_id -> vect

  (** [make b f] creates the sum of [(f s Fi)*Fi]
      over all flags [Fi] in the basis [b],
      where [s] is the size of the type (i.e. [s = b.typeSize]). *)
  val make : ?name:string -> Flag.t basis_id -> (int -> Flag.t -> F.t) -> vect

  (** Draws a vector as linear combinaison of pictures
      with the Graphics library. *)
  val draw : vect -> unit
end
      
