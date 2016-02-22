(** Identifying, computing, storing and accessing precomputed tables *)

(** A ['a] basis_id object [b] refers to a subspace of all
    flags (with ocaml type ['a]) with a size [b.flagSize] and
    with type the [b.typeId]-th flag of size [b.typeSize]. *)
type 'a basis_id = {
  flagType : string;
  flagSize : int;
  typeSize : int;
  typeId : int
}

(** Encodes the basis into a string. *)
val basis_name : 'a basis_id -> string

(* Operations on bases *)
val untype_basis : 'a basis_id -> 'a basis_id
val mul_basis : 'a basis_id -> 'a basis_id -> 'a basis_id
val square_basis : 'a basis_id -> 'a basis_id
val div_basis : 'a basis_id -> 'a basis_id -> 'a basis_id

module Make :
  functor ( Flag : Flag.S ) ->
sig
  open Flag

  (** {8 Creating a basis for this algebra } *)
  
  val basis_id : int -> int -> int -> t basis_id
  val untyped_basis_id : int -> t basis_id

  (** {8 Compute or load functions } *)
				  
  val get_basis : t basis_id -> t array
  val get_p : t basis_id -> t basis_id -> int array array
  val get_p2 : t basis_id -> t basis_id -> t basis_id -> int array array array
  val get_q : t basis_id -> int array
  val get_untype : t basis_id -> int array
  val get_size : t basis_id -> int

  val get_p_denom : t basis_id -> t basis_id -> int
  val get_p2_denom : t basis_id -> t basis_id -> t basis_id -> int
  val get_q_denom : t basis_id -> int
    
  val id_flag : t basis_id -> t -> int

end
