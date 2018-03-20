(** Operations on inequalities in flag algebras. *)

open Storage
open Vectors

(** The type of lower bounds on a sum of flags of ocaml type ['b]
with scalar coefficients of type ['a].
The meaning of such an object [ineq] is "[ineq.flags] >= [ineq.bound]".
 *)
type ('a, 'b) inequality = {
  name : string option;
  flags : ('a, 'b) Vectors.vector;
  bound : 'a;
  boundName : string option
}

(** [map f ineq] applies [f] on scalar coefficients of [ineq] and returns the result *)
val map : ('a -> 'b) -> ('a, 'c) inequality -> ('b, 'c) inequality

val print : ('a -> string) -> ('a, 'b) inequality -> string

module Make :
  functor (F : Field.S) ->
    functor (Flag : Flag.S) ->
sig
  (** Shortcut for inequalities of this set. *)
  type ineq = (F.t, Flag.t) inequality
 
  (** [opposite ineq] where [ineq] means "f >= x" returns "-f >= -x" (i.e. "f <= x"). *)
  val opposite : ineq -> ineq

  (** [equality ineq] where [ineq] means "f >= x"
      returns the list \["f >= x"; "f <= x"\]
      (so that their conjunction means "f = x"). *)
  val equality : ?epsilon:F.t ->  ineq -> ineq list
  
  (** [untype ineq] where [ineq] means "f >= x" and f is a rooted
      quantum flag returns the unrooted projection "\[f\] >= x" of the
      inequality. *)
  val untype : ineq -> ineq

  (** [expand b ineq] expresses [ineq]
      on the basis [b] of flags of larger size. *)
  val expand : Flag.t basis_id -> ineq -> ineq

  (** [multiply_by_flag b i ineq]
      where [ineq] means "f >= x"
      returns the inequality "f*H -x*H >= 0" (i.e. "f*H >= x*H")
      where H is the [i]-th flag in basis [b]. *)
  val multiply_by_flag :
    Flag.t basis_id -> int -> ineq -> ineq

   (** [multiply_by_all_flags b ineq]
       returns the the list of all inequalities
       obtained by multiplication of
       flags of [b] with [ineq]. *)
  val multiply_by_all_flags :
    Flag.t basis_id -> ineq -> ineq list

  (** [multiply_and_unlabel b ineq]
       returns the the list of all inequalities
       multiplied with the multiplication of
       all suitable flag  with [ineq] and unlabeling
       such that the results are in basis [b].*)
  val multiply_and_unlabel :
    Flag.t basis_id -> ineq -> ineq list
				    
  (** [totalsum b] returns the ineqality :
      sum over all H in basis b is at least 1. *)
  val totalsum : Flag.t basis_id -> ineq

  (** [at_least f x] creates the inequality "[f] >= [x]". *)
  val at_least : (F.t, Flag.t) vector -> F.t -> ineq

  (** Same as {!at_least} with "[f] <= [x]". *)
  val at_most : (F.t, Flag.t) vector -> F.t -> ineq

  (** [flag_at_least b i x] returns "H >= x"
      where H is the [i]-th flag of the basis [b]. *)
  val flag_at_least : Flag.t basis_id -> int -> F.t -> ineq

  (** [flag_at_most b i x] returns "H <= x"
      where H is the [i]-th flag of the basis [b]. *)
  val flag_at_most : Flag.t basis_id -> int -> F.t -> ineq

  (** [less_than f g] returns the inequality "f <= g" *)		
  val less_than : (F.t, Flag.t) vector -> (F.t, Flag.t) vector -> ineq
							
  (** [all_flags_nonnegative b] returns the list of
      inequalities of the form "H >= 0"
      where H is a flag of the basis b. *)
  val all_flags_nonnegative : Flag.t basis_id -> ineq list

  (** Set the name field of a list of inequalities. *)
  val name_list : string -> ineq list -> ineq list
end
