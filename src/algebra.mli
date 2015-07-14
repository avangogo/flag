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
    
  val iter_on_automorphisms : (int array -> unit) -> Flag.t -> unit
  val automorphisms : Flag.t -> int array list
    
  val count_subflags : int -> Flag.t -> Flag.t -> int
    
  (** {8 Rasborov functions (subflags densities) } *)
  val p : int -> Flag.t -> Flag.t -> Rational.t
  val p2 : int -> Flag.t -> Flag.t -> Flag.t -> Rational.t
  val q : int -> Flag.t -> Rational.t

  val p2_tabulate : int -> Flag.t array ->
    Flag.t array -> Flag.t array -> Rational.t array array array


  (** {8 Flags generation } *)
  val span_flags : int -> Flag.t list
  val list_span_typed : Flag.t -> Flag.t -> Flag.t list
  val span_all_typed : Flag.t -> Flag.t list -> Flag.t list
  val span_flags_next : Flag.t list -> Flag.t list
  val span_flags_iterative : int -> Flag.t list

end
