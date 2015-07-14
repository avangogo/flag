open Storage
open Rational
open Sdp
open Common
open Vectors

(* Notations : In this module, *)
(* i is always an inequaliy *)
(* b is always a flag basis *)
 
(* means flags >= bound *)
type ('a, 'b) inequality =
  { name : string option;
    flags : ('a, 'b) vector;
    bound : 'a }

(* operations on inequalities *)
let map f i =
  { name = i.name;
    flags = Vectors.map f i.flags ;
    bound = f i.bound }

let print print_scal i =
  Printf.sprintf "%s >= %s"
		 (*		 (option_default "." i.flags.Vectors.name) (print_scal i.bound) *)
		 (Prettyprinting.print i.flags.Vectors.expr) (print_scal i.bound)

module Make ( F : Field.S ) ( Flag : Flag.S ) =
  struct
    
  module Vect = Vectors.Vect (F) (Flag)

  type ineq = (F.t, Flag.t) inequality

  let opposite i =
    { name = option_map (Printf.sprintf "%s(-)") i.name;
      flags = Vect.opposite i.flags;
      bound = F.minus i.bound }
    
  let equality i =
    let name = option_map (Printf.sprintf "%s (Equality)") i.name in
    [ { i with name = name };
      { (opposite i) with name = name } ]

  let untype i =
    {
      name = option_map (Printf.sprintf "%s(untyped)") i.name;
      flags = Vect.untype i.flags;
      bound = i.bound
    }

  (* change of basis *)
  let expand b i =
    {
      name = option_map (Printf.sprintf "%s(expanded)") i.name;
      flags = Vect.expand b i.flags;
      bound = i.bound
    }
      
(*  let multiply_by_flag_without_bound b flagId i =
    assert (i.bound = F.zero);
    let flag = Vect.flag_of_id b flagId in
    {
      name = None;
      flags = Vect.multiply flag i.flags;
      bound = i.bound
    }*)

  let multiply_by_flag b flagId i =
    let flag = Vect.flag_of_id ~name:"x" b flagId in
    let new_b =  mul_basis b i.flags.basis in
    let exp_flag = Vect.expand new_b flag in
    {
      name = None;
      flags =
	Vect.sub (Vect.multiply flag i.flags)
	  (Vect.scalar_mul i.bound exp_flag);
      bound = F.zero
    }

  let multiply_by_all_flags b i =
    let b_size = Vect.get_size b in
    list_init b_size (fun id -> multiply_by_flag b id i)

  let multiply_and_unlabel b i =
    let b0 = i.flags.basis in
    let b1 = Storage.div_basis {b with typeSize = b0.typeSize} b0 in
    let i2 = multiply_by_all_flags b1 i in
    List.map untype i2
	      
  let totalsum b =
    { name = Some "Total sum is 1";
      flags = Vect.one b;
      bound = F.int 1 }
      
  let at_least flags bound =
    { name = None;
      flags = flags;
      bound = bound }

  let at_most flags bound =
    { name = None;
      flags = Vect.opposite flags;
      bound = F.minus bound }

  let flag_at_least b flagid bound =
    { name = None;
      flags = Vect.flag_of_id b flagid;
      bound = bound }

  let flag_at_most b flagid bound =
    opposite (flag_at_least b flagid bound)

  let less_than flags1 flags2 =
    { name = None;
      flags = Vect.sub flags2 flags1;
      bound = F.zero }
      
  let flag_nonnegative b flagid =
    let name = Some "Flag is non-negative" in
    { (flag_at_least b flagid F.zero) with name = name }

  let all_flags_nonnegative b =
    let n = Vect.get_size b in
    list_init n (flag_nonnegative b)

  let name_list name l =
    let someName = Some name in
    List.map (fun i -> { i with name = someName }) l

end
