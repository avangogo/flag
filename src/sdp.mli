(** Structures and functions for sdp programming. *)

type block_type = MAT of int | DIAG of int

val block_size : block_type -> int

type 'a sparseblock = {
  entries : 'a array;
  iindices : int array;
  jindices : int array;
}

type 'a matrix = 'a sparseblock array

val empty_sparseblock : 'a sparseblock
val sparseblock_of_matrix : 'a -> 'a array array -> 'a sparseblock
val sparseblock_of_diag : 'a -> 'a array -> 'a sparseblock

val matrix_of_sparseblock : 'a -> int -> 'a sparseblock -> 'a array array

val sparseblock_map : ('a -> 'b) -> 'a sparseblock -> 'b sparseblock

                                                         
(** The problem (t, C, (Ai), (ai)) corresponds to
maximizing A ** X 
under Ai ** X = ai for every i
and X s.d.p.
t is the array of types of the blocks *)                  
type sdp_problem =
  block_type array * float matrix * float matrix array * float array

(* write_prob file c ctype a b *)
val write_prob :
  string -> sdp_problem -> unit

val load_certificate : string -> float array * float matrix array

val multiply_matrices_float : float matrix -> float matrix -> float
