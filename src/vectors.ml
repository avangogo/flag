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
  let get_q b = Array.map F.rat (S.get_q b)
  let get_untype = S.get_untype
  let get_p b1 b2 = matrix_map F.rat (S.get_p b1 b2)
  let get_p2 b1 b2 b3 = Array.map (matrix_map F.rat) (S.get_p2 b1 b2 b3)
  let get_size = S.get_size
  let get_basis = S.get_basis
    
  (* operations on vectors *)
  (* removing the type *)
  let raw_untype b v =
    let q = get_q b
    and untype = get_untype b
    and n0 = get_size (untype_basis b) in
    let res = Array.make n0 F.zero in
    for i = 0 to (Array.length v) - 1 do
      res.(untype.(i)) <- F.add res.(untype.(i)) (F.mul q.(i) v.(i))
    done;
    res
      
  let untype a =
    { name = option_map (sprintf "[| %s |]") a.name;
      expr = Unlab a.expr;
      basis = untype_basis a.basis;
      vect = raw_untype a.basis a.vect }

  (* change of basis *)
  let raw_expand b1 b2 v =
    let p = get_p b1 b2 in
    (multiply_matrix F.zero F.add F.mul [|v|] p).(0)

  let expand new_basis a =
    { name = a.name;
      expr = a.expr;
      basis = new_basis;
      vect = raw_expand a.basis new_basis a.vect }

  (*  *)
  let raw_multiply b1 b2 v1 v2 =
    let b = mul_basis b1 b2 in
    let p2 = get_p2 b1 b2 b in
    let p2_inv = matrix_array_of_array_matrix p2 in
    Array.map (fun m -> bilinear_form F.zero F.add F.mul v1 m v2) p2_inv
      
  let multiply a b =
    { name = option_map2 (sprintf "%s*%s") a.name b.name;
      expr = Times (a.expr, b.expr);
      basis = mul_basis a.basis b.basis;
      vect = raw_multiply a.basis b.basis a.vect b.vect }

  let raw_scalar_mul lambda v =
    Array.map (F.mul lambda) v

  let scalar_mul ?name lambda a =
    let scalar_name = option_default (F.print lambda) name in
    { name = option_map (sprintf "%s.%s" scalar_name) a.name;
      expr = Times (Num scalar_name, a.expr);
      basis = a.basis;
      vect = raw_scalar_mul lambda a.vect }

  let raw_add v1 v2 =
    array_map2 F.add v1 v2

  let add a b =
    { name = option_map2 (sprintf "(%s + %s)") a.name b.name;
      expr = Plus (a.expr, b.expr);
      basis = a.basis;
      vect = raw_add a.vect b.vect }

  let raw_sub v1 v2 =
    array_map2 F.sub v1 v2

  let sub a b =
    { name = option_map2 (sprintf "(%s - %s)") a.name b.name;
      expr = Plus (a.expr, Minus b.expr);
      basis = a.basis;
      vect = raw_sub a.vect b.vect }    

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