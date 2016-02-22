(** Field signature. *)

module type S =
sig
  type t
  val zero : t
  val rat : Rational.t -> t
  val int : int -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val minus : t -> t
  val print : t -> string
end

module Float : S with type t = float =
struct
  type t = float
  let zero = 0.
  let int = float_of_int
  let rat r =
    (int (Rational.nom r)) /. (int (Rational.denom r))
  let mul = ( *.)
  let div = ( /.)
  let add = ( +.)
  let sub = ( -.)
  let minus = (~-.)
  let print = string_of_float
end

module Num : S with type t = Num.num =
struct
  open Num
  type t = num
  let int i = Int i
  let zero = int 0
  let rat r =
    (int (Rational.nom r)) // (int (Rational.denom r))
  let mul = ( */ )
  let div = ( // )
  let add = ( +/ )
  let sub = ( -/ )
  let minus = minus_num
  let print = string_of_num
end
