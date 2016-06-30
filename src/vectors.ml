open Common
open Storage
open Printf
open Prettyprinting
   
(* represent sum_i vect.(i) basis.(i) *)
type ('a, 'b) vector =
  { 
    name : string option;
    expr : expr;
    basis : 'b basis_id;
    vect : 'a array;
  }

let map f a =
  { a with vect = Array.map f a.vect }

(* factorisation of rational and float algorithms *)
module Vect ( F : Field.S ) ( Flag : Flag.S ) =
struct
  
  type vect = (F.t, Flag.t) vector

  module S = Storage.Make (Flag)

  (*  *)
  let get_q b = S.get_q b (* FIXME *)
  let get_untype = S.get_untype
  let get_p b1 b2 = S.get_p b1 b2 (* FIXME *)
  let get_p2 b1 b2 b3 = S.get_p2 b1 b2 b3 (* FIXME *)
  let get_size = S.get_size
  let get_basis = S.get_basis
    
  (* operations on vectors *)
  (* removing the type *)
  let raw_untype b v =
    let q = get_q b
    and denom = S.get_q_denom b
    and untype = get_untype b
    and n0 = get_size (untype_basis b) in
    let res = Array.make n0 F.zero in
    for i = 0 to (Array.length v) - 1 do
      res.(untype.(i)) <- F.add res.(untype.(i)) (F.mul (F.int q.(i)) v.(i))
    done;
    Array.map (fun x -> F.div x (F.int denom)) res
      
  let untype a =
    { name = option_map (sprintf "[| %s |]") a.name;
      expr = Unlab a.expr;
      basis = untype_basis a.basis;
      vect = raw_untype a.basis a.vect }
      
  (* change of basis *)
  let raw_expand (p : 'a option) b1 b2 v =
    let p = match p with
      | Some x -> x
      | None -> S.get_p b1 b2 in
    let denom = S.get_p_denom b1 b2 in
    let scaled_v = Array.map (fun x -> F.div x (F.int denom)) v in
    Sparse.field_apply F.int F.zero F.add F.mul p scaled_v
    
  let vect_expand ?p new_basis a =
    { name = a.name;
      expr = a.expr;
      basis = new_basis;
      vect = raw_expand p a.basis new_basis a.vect }

  let expand new_basis a =
    vect_expand new_basis a

  let expand_all new_b t =
    let b = t.(0).basis in (* FIXME : error if t is empty *)
    assert (array_for_all (fun v -> v.basis = b) t);
    let p = S.get_p b new_b in
    Array.map (vect_expand ~p:p new_b) t
      
  (* Multiplication *)

  let raw_multiply (p2 : 'a option ) b1 b2 v1 v2 =
    let b = mul_basis b1 b2 in
    let p2 = match p2 with
      | Some x -> x
      | None -> S.get_p2 b1 b2 b in
    let denom = S.get_p2_denom b1 b2 b in
    Array.map (fun m ->
      F.div
	(Sparse.field_bilinear F.int F.zero F.add F.mul v1 m v2)
	(F.int denom)
    ) p2
      
  let vect_multiply ?p2 a b =
    { name = option_map2 (sprintf "%s*%s") a.name b.name;
      expr = Times (a.expr, b.expr);
      basis = mul_basis a.basis b.basis;
      vect = raw_multiply p2 a.basis b.basis a.vect b.vect }

  let multiply a b =
    vect_multiply a b

  let ( *~ ) = multiply
      
  let multiply_all t1 t2 =
    let b1 = t1.(0).basis in
    let b2 = t2.(0).basis in
    assert (array_for_all (fun v -> v.basis = b1) t1);
    assert (array_for_all (fun v -> v.basis = b2) t2);
    let p2 = S.get_p2 b1 b2 (mul_basis b1 b2) in
    let init i j =
      vect_multiply ~p2:p2 t1.(i) t2.(j) in
    Array.init (Array.length t1) (fun i -> Array.init (Array.length t2) (init i))
      
  let raw_scalar_mul lambda v =
    Array.map (F.mul lambda) v

  let scalar_mul ?name lambda a =
    let scalar_expr = match name with
      | None -> expr F.print F.int lambda
      | Some name -> Num name in
    { name = option_map (sprintf "%s.%s" (print scalar_expr)) a.name;
      expr = Times (scalar_expr, a.expr);
      basis = a.basis;
      vect = raw_scalar_mul lambda a.vect }
      
  let raw_add v1 v2 =
    array_map2 F.add v1 v2

  let add a b =
    assert (a.basis = b.basis);
    { name = option_map2 (sprintf "(%s + %s)") a.name b.name;
      expr = Plus (a.expr, b.expr);
      basis = a.basis;
      vect = raw_add a.vect b.vect }

  let ( +~ ) = add 
      
  let raw_sub v1 v2 =
    array_map2 F.sub v1 v2

  let sub a b =
    assert (a.basis = b.basis);
    { name = option_map2 (sprintf "(%s - %s)") a.name b.name;
      expr = Plus (a.expr, Minus b.expr);
      basis = a.basis;
      vect = raw_sub a.vect b.vect }    

  let ( -~ ) = sub
      
  let raw_opposite v =
    Array.map F.minus v

  let opposite a =
    { name = option_map ((^) "-") a.name;
      expr = Minus a.expr;
      basis = a.basis;
      vect = raw_opposite a.vect }

  (* *)
  let raw_flag basis i =
    let n = get_size basis in
    let r = Array.make n F.zero in
    r.(i) <- F.int 1;
    r

  let flag_of_id ?name basis i =
    { name = name;
      expr = Flag (option_default "." name);
      basis = basis;
      vect = raw_flag basis i }

  let flag ?name basis f =
    let i = S.id_flag basis f in
    { (flag_of_id basis i) with name = name; expr = Flag (option_default "." name) }

  (* *)
  let empty basis =
    let n = get_size basis in
    { name = Some "0";
      expr = Zero;
      basis = basis;
      vect = Array.make n F.zero }

  let one basis =
    let n = get_size basis in
    { name = Some "1";
      expr = One;
      basis = basis;
      vect = Array.make n (F.int 1) }

  let make ?name basis f =
    { name = name;
      expr = Flag (option_default "." name);
      basis = basis;
      vect = Array.map (f basis.typeSize) (get_basis basis) }

  let draw v =
    Graphic.draw_sum F.print F.zero (Flag.draw ~root:v.basis.typeSize) v.vect (get_basis v.basis)

end
