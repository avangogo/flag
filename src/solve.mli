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
    functor (F : Field.S) -> 
sig

  (** Gives the list of bases that can be used for
  cauchy-schwartz inequalities for a problem expressed in
  the input basis.*)
  val all_cs : Flag.t basis_id -> Flag.t basis_id list

  (** [solve filename cs ineq obj] build the sdp problem
      expressed on the flag basis [obj.basis]
      with inequalities [ineq] as constraints,
      cauchy-schwartz blocks for all bases of [b] and objective value
      [obj] and writes it to file [filename] in sdpa format.
      If [Param.latex] is enable, it also writes it in latex format in
      file latex/filename.tex . *)
  val solve :
    string ->
    Flag.t basis_id list ->
    (F.t, Flag.t) Inequality.inequality list ->
    (F.t, Flag.t) Vectors.vector -> unit

end
