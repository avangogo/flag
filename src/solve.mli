(** Main generic function for building sdpa instances *)

open Storage
open Sdp

type 'a constraintblock = {
  block_type : block_type;
  bflags : 'a sparseblock array;
  bbound : 'a sparseblock;
}

val constraintblock_map :
  ('a -> 'b) -> 'a constraintblock -> 'b constraintblock

(*
val denom_q : 'a basis_id -> int
val denom_p : 'a basis_id -> 'b basis_id -> int
val denom_p2_square : 'a basis_id -> int
*)

module Make :
  functor (Flag : Flag.S) ->
sig

  (** Gives the list of bases that can be used for
  cauchy-schwartz inequalities for a problem expressed in
  the input basis.*)
  val all_cs : Flag.t basis_id -> Flag.t basis_id list

  (** [solve filename b cs rat_ineqs float_ineqs obj] build the sdp problem
      expressed on the flag basis [b]
      with all constraints of [rat_ineqs] and [float_ineqs] (the only
      difference is between them is the type of their coefficient),
      cauchy-schwartz blocks for all bases of [b] and objective value
      [obj] and writes it to file [filename] in sdpa format.
      If [Param.latex] is enable, it also writes it in latex format in
      file latex/filename.tex . *)
  val solve :
    string ->
    Flag.t basis_id ->
    Flag.t basis_id list ->
    (Rational.t, Flag.t) Inequality.inequality list ->
    (float, Flag.t) Inequality.inequality list ->
    Rational.t array -> unit

end
