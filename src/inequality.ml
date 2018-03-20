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
    bound : 'a;
    boundName : string option
  }

(* operations on inequalities *)
let map f i =
  { name = i.name;
    flags = Vectors.map f i.flags ;
    bound = f i.bound;
    boundName = None
  }

let print print_scal i =
  Printf.sprintf "%s >= %s"
		 (Prettyprinting.print i.flags.Vectors.expr)
                 (option_default (print_scal i.bound) i.boundName)

module Make ( F : Field.S ) ( Flag : Flag.S ) =
  struct
    
  module Vect = Vectors.Vect (F) (Flag)

  type ineq = (F.t, Flag.t) inequality

  let opposite i =
    { name = option_map (Printf.sprintf "%s(-)") i.name;
      flags = Vect.opposite i.flags;
      bound = F.minus i.bound;
      boundName = option_map (Printf.sprintf "%s(-)") i.boundName }

  let relax epsilon i =
    { i with bound = F.sub i.bound epsilon }
      
  let equality ?epsilon:(e=F.zero) i =
    let name = option_map (Printf.sprintf "%s (Equality)") i.name in
    [ relax e { i with name = name };
      relax e { (opposite i) with name = name } ]

  let untype i =
    {
      name = option_map (Printf.sprintf "%s(untyped)") i.name;
      flags = Vect.untype i.flags;
      bound = i.bound;
      boundName = i.boundName
    }

  (* change of basis *)
  let expand b i =
    {
      name = option_map (Printf.sprintf "%s (expanded)") i.name;
      flags = Vect.expand b i.flags;
      bound = i.bound;
      boundName = i.boundName
    }

  (* Translate the scalar bound into a flag *)
  let bound_flag i =
    let res = Vect.scalar_mul i.bound (Vect.one i.flags.basis) in
    let name, expr = match i.boundName
      with Some x -> Some x, Prettyprinting.Num x
         | None -> res.name, res.expr in
    { res with Vectors.name = name; Vectors.expr = expr }
      
  let multiply_by_flag b flagId i =
    let flag = Vect.flag_of_id ~name:"x" b flagId in
    {
      name = None;
      flags = Vect.multiply flag (Vect.sub i.flags (bound_flag i));
      bound = F.zero;
      boundName = None
    }

  let multiply_by_all_flags b i =
    let i_flags =
      (Vect.sub i.flags (Vect.scalar_mul i.bound (bound_flag i))) in    
    let b_size = Vect.get_size b in
    let basis = Array.init b_size (Vect.flag_of_id ~name:"x" b) in
    let products = Vect.multiply_all [| i_flags |] basis in
    Array.to_list (Array.init b_size (fun k -> 
      {
	name = None;
	flags = products.(0).(k);
	bound = F.zero;
        boundName = None
      }
    ))

  let multiply_and_unlabel_old b i =
    let b0 = i.flags.basis in
    let b1 = Storage.div_basis
      {b with typeSize = b0.typeSize; typeId = b0.typeId} b0 in
    let i2 = multiply_by_all_flags b1 i in
    List.map untype i2

  (* FIXME *)
  let multiply_and_unlabel b i =
    let b0 = i.flags.basis in
    let b1 = Storage.div_basis
               {b with typeSize = b0.typeSize; typeId = b0.typeId} b0 in
    let i_flags = Vect.sub i.flags (bound_flag i) in    
    let b_size = Vect.get_size b1 in
    let basis = Array.init b_size (Vect.flag_of_id ~name:"x" b1) in
    let products = Vect.multiply_and_unlabel_all [| i_flags |] basis in
    Array.to_list (Array.init b_size (fun k -> 
                                {
	                          name = None;
	                          flags = products.(0).(k);
	                          bound = F.zero;
                                  boundName = None
                                }
                  ))
             
  let totalsum b =
    { name = Some "Total sum is 1";
      flags = Vect.one b;
      bound = F.int 1;
      boundName = None }
      
  let at_least flags bound =
    { name = None;
      flags = flags;
      bound = bound;
      boundName = None }

  let at_most flags bound =
    { name = None;
      flags = Vect.opposite flags;
      bound = F.minus bound;
      boundName = None }

  let flag_at_least b flagid bound =
    { name = None;
      flags = Vect.flag_of_id ~name:"x" b flagid;
      bound = bound;
      boundName = None }

  let flag_at_most b flagid bound =
    opposite (flag_at_least b flagid bound)

  let less_than flags1 flags2 =
    { name = None;
      flags = Vect.sub flags2 flags1;
      bound = F.zero;
      boundName = None }
      
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
