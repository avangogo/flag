(** Sparse integer matrices. *)

type t
(** A type for sparse matrices *)

val n : t -> int
val m : t -> int
  
val iter : (int -> int -> int -> unit) -> t -> unit
(** [iter f m] applies the function f (as [f i j v]) to the indices
    [i], [j] and the value [v] of each non-zero entry of [m] *)

val map : (int -> int) -> t -> t

val map_in_place : (int -> int) -> t -> unit
(** [map f m] modify [m] to replace each non-zero value [v] by [f v] *)
  
val apply : t -> int array -> int array
(** matrix-vector standard multiplication.
    [apply m v] returns [m] times [v] *)

val field_apply :
  (int -> 'a) -> 'a -> ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> t -> 'a array -> 'a array
(** Usage : [apply int zero add mul m v] *)

val mul : t -> t -> t
(** matrix-matrix standard multiplication.
    [mul m1 m2] returns [m1] times [m2] *)
  
val semimul : t -> t -> t
(** [semimul m1 m2] returns [m1] times the transpose matrix of [m2] *)

val bilinear : int array -> t -> int array -> int
(** apply a matrix as a linear form.
    [bilinear_form v1 m v2] returns (transpose [v1]) times [m] times [v2] *)

val get : t -> int -> int -> int
(** [get m i j] returns m(i,j) at the cost of binary search.*)

val field_bilinear :
  (int -> 'a) -> 'a -> ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) ->
  'a array -> t -> 'a array -> 'a


(** [linear_combination select coeff t] computes the matrix sum :
    coeff.(select.(0))*t.(select.(0)) +...+ coeff.(select.(m-1))*t.(select.(m-1))*)
val linear_combination : int array -> int array -> t array -> t
  
val to_dense : t -> int array array

val to_sparseblock : t -> int Sdp.sparseblock
  
val of_dense : int array array -> t
  
(** {8 Creating sparse matrices }
    
    The functions of this section make it possible to create a sparse matrix in an efficient way.
    To create a matrix,
    first call [make_dynmat] and [makeSPA] to create a matrix accumulator [m]
    and a line accumulator [l]

    Then for each line, call [scatterSPA] to add a values to [l]
    (in any order, multiples values to same indice will be sumed).
    Call [gatherSPA] at the end of the line [l] to [m].

    At the end, call [dynmat_to_sparse] to convert [m] into a sparse matrix.
*) 
  
type dynmat
(** A type for an accumulator matrix where lines can be add *)
  
val make_dynmat : int -> int -> dynmat
(** [make_dynat n m] create an empty matrix accumulator
    with [n] rows and [m] column *)
  
val dynmat_to_sparse : dynmat -> t
(** Convert to a sparse matrix *)
  
type spa
(** Accumulator for lines *)
  
val makeSPA : int -> spa
(** [makeSPA n] reate an accumulator for lines of size [n] *)
  
val scatterSPA : spa -> int -> int -> unit
(** [scatterSPA acc v j] add the value [v] to position [j] of the line *)
  
val gatherSPA : spa -> dynmat -> unit
(** [gatherSPA acc m] add the line stored in [acc] to the matrix [m]
    and empty the accumulator*)
