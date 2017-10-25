(** General algorithms on flags (Normal forms, automorphisms and densities). *)
module Make :
  functor ( Flag : Flag.S ) ->
sig

  type flag = Flag.t
  val flagname : string

  (** {8 Normal form algorithms } *)
  val normal_form_old : Flag.t -> Flag.t
  val normal_form : Flag.t -> Flag.t
  val normal_form_typed : int -> Flag.t -> Flag.t
  val bad_normal_form_typed_morphism : int -> Flag.t -> int array
    
  val iter_on_automorphisms : (int array -> unit) -> Flag.t -> unit
  val automorphisms : Flag.t -> int array list
    
  val count_subflags : int -> Flag.t -> Flag.t -> int
    
  (** {8 Rasborov functions (subflags densities) } *)
  val p : int -> Flag.t -> Flag.t -> Rational.t
  val p2 : int -> Flag.t -> Flag.t -> Flag.t -> Rational.t
  val q : int -> Flag.t -> Rational.t
                             
  (** Optimized functions *)
    
  val p2_tabulate : int -> Flag.t array ->
    Flag.t array -> Flag.t array -> Sparse.t array
  val p_tabulate : int -> Flag.t array -> Flag.t array -> Sparse.t

  val q_nom : int -> Flag.t -> int
    
  val p_denom : int -> int -> int -> int
  val p2_denom : int -> int -> int -> int
  val q_denom : int -> int -> int
  val part_q_denom : int -> int -> int -> int
                                
  val canonical_untype_tabulate : int -> Flag.t -> Flag.t array -> Flag.t array -> int array

  (** {8 Flags generation } *)
  val span_flags : int -> Flag.t list
  val list_span_typed : Flag.t -> Flag.t -> Flag.t list
  val span_all_typed : Flag.t -> Flag.t list -> Flag.t list
  val span_flags_next : Flag.t list -> Flag.t list
  val span_flags_iterative : int -> Flag.t list

end
