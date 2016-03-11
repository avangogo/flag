type 'a sparse =
  {
    n : int ; (* number of columns *)
    m : int ; (* number of lines *)
    v : 'a array ; (* values *)
    j : int array ; (* column indice *)
    indi : int array  (* indi.(i) is the first indice of the line i *)
  (* indi has size n+1 and  we assume indj.(nb) = n *)
  }

type t = int sparse

let n mat =
  mat.n

let m mat =
  mat.m    
  
let iter f mat =
  for i = 0 to mat.n-1 do
    for k = mat.indi.(i) to mat.indi.(i+1)-1 do
      f i mat.j.(k) mat.v.(k)
    done
  done

let map f mat =
  { mat with v = Array.map f mat.v }
    
let map_in_place f mat =
  for i = 0 to ( Array.length mat.v ) - 1 do
    mat.v.(i) <- f mat.v.(i)
  done
    
let make n m nb a = {
  n = n ;
  m = m ;
  v = Array.make nb a;
  j = Array.make nb (-1);
  indi = Array.make (n+1) (-1);
}

let get m i j =
  let rec search min sup =
    if m.j.(min) = j then m.v.(min)
    else if min = sup then 0
    else
      let mid = ( min + sup )/2 in
      if j < m.j.(mid) then search min m.j.(mid)
      else search m.j.(mid) sup in
  search m.indi.(i) m.indi.(i+1)
  
(** Multiplication with a dense vector *)

let field_apply int zero add mul mat vect =
  let res = Array.make mat.n zero in
  iter (fun i j v -> res.(i) <- add res.(i) ( mul vect.(j) (int v) ) ) mat;
  res

let apply mat vect =
  field_apply (fun x -> x) 0 ( + ) ( * ) mat vect
let array_of_revlist n list =
  let res = Array.make n 0 in
  List.iteri ( fun i a -> res.(n-i-1) <- a ) list;
  res

(* dynamic matrix for sparse matrix creation *)
type dynmat =
  {
    d_n : int ;
    d_m : int ;
    mutable d_nb : int ;
    mutable d_i : int ;
    mutable d_v : int list ;
    mutable d_j : int list ;
    d_indi : int array
  }

let make_dynmat n m =
  {
    d_n = n ;
    d_m = m ;
    d_nb = 0 ;
    d_v = [] ;
    d_i = 0;
    d_j = [] ;
    d_indi = Array.make (n+1) 0 
  }
    
let dynmat_add_value m j v =
  m.d_j <- j::m.d_j;
  m.d_v <- v::m.d_v;
  m.d_nb <- m.d_nb + 1
    
let dynmat_to_sparse m =
  assert ( m.d_i = m.d_n );
  m.d_indi.(m.d_n) <- m.d_nb;
  {
    n = m.d_n ;
    m = m.d_m ;
    v = array_of_revlist m.d_nb m.d_v;
    j = array_of_revlist m.d_nb m.d_j;
    indi = m.d_indi
  }
    
(* sparse accumulator *)
type spa =
  {
    w : int array;
    b : bool array;
    mutable ls : int list
  }
    
let makeSPA n =
  {
    w = Array.make n 0;
    b = Array.make n false;
    ls = [];
  }

let scatterSPA spa v pos =
    if not spa.b.(pos) then
      begin
	spa.b.(pos) <- true;
	spa.w.(pos) <- v;
	spa.ls <- pos :: spa.ls;
      end
    else
      spa.w.(pos) <- spa.w.(pos) + v

let gatherSPA spa dynmat =
  let add pos =
    dynmat_add_value dynmat pos spa.w.(pos);
    spa.b.(pos) <- false in
  List.iter add (List.sort compare spa.ls);
  spa.ls <- [];
  (* new line *)
  dynmat.d_i <- dynmat.d_i + 1;
  dynmat.d_indi.(dynmat.d_i) <- dynmat.d_nb
	
let mul a b =
  let spa = makeSPA b.m in
  let res = make_dynmat a.n b.m in
  
  for ia = 0 to a.n-1 do (* pour toute ligne de a *)
    for ka = a.indi.(ia) to a.indi.(ia+1)-1 do
      for kb = b.indi.(a.j.(ka)) to b.indi.(a.j.(ka)+1)-1 do
	let value = a.v.(ka) * b.v.(kb) in
	scatterSPA spa value b.j.(kb)
      done;
    done;
    gatherSPA spa res      
  done;
  dynmat_to_sparse res    
    
let semimul a b = (* computes a*(transpose b) *)

  let res = make_dynmat a.n b.n in
  
  for ia = 0 to a.n-1 do (* pour toute ligne de a *)
    for ib = 0 to b.n-1 do (* pour toute ligne de b *)    

      let resij = ref 0 in
      let ka = ref a.indi.(ia)
      and kb = ref b.indi.(ib)
      and maxka = a.indi.(ia+1)
      and maxkb = b.indi.(ib+1) in

      while (!ka < maxka && !kb < maxkb ) do
	let ja = a.j.(!ka) and jb = b.j.(!kb) in
	if ja = jb then
	  begin
	    resij := !resij + a.v.(!ka) * b.v.(!kb);
	    incr ka;
	    incr kb
	  end
	else if ja < jb then
	  incr ka
	else
	  incr kb
      done ;
      if !resij <> 0 then
	dynmat_add_value res ib !resij
    done
  done;

  dynmat_to_sparse res

let bilinear x m y =
  let res = ref 0 in
  iter (fun i j v -> res := x.(i) * v * y.(j) + !res) m;
  !res

let field_bilinear int zero add mul x m y =
  let res = ref zero in
  iter (fun i j v -> res := add (mul (mul x.(i) (int  v)) y.(j)) !res) m;
  !res

(* computes the matrix sum :
   coeff.(select.(0))*t.(select.(0)) +...+ coeff.(select.(m-1))*t.(select.(m-1))*)
let linear_combination select coeff t =
  let n = t.(0).n in
  let m = t.(0).m in
  let res = make_dynmat n m in
  let spa = makeSPA m in
  (* for each line *)
  for i = 0 to n - 1 do
    (* for each selected matrix *)
    for k = 0 to (Array.length select) - 1 do
      let m = t.(select.(k)) in
      (* for each entry of this line *)
      for p = m.indi.(i) to m.indi.(i+1)-1 do
	(* add the line to the accumulator *)
	scatterSPA spa (m.v.(p) * coeff.(select.(k))) m.j.(p);
      done;
    done;
    gatherSPA spa res;
  done;
  dynmat_to_sparse res
  
(* dense matrices for testing *)
let dense_count_values mat =
  let res = ref 0 in
  for i = 0 to (Array.length mat) - 1 do
    for j = 0 to (Array.length mat.(0)) - 1 do
      if mat.(i).(j) <> 0 then incr res
    done
  done;
  !res

let of_dense mat =
  let n = Array.length mat
  and m = Array.length mat.(0) in
  let nb = dense_count_values mat in
  let res = make n m nb 0 in
  res.indi.(n) <- nb; (**)
  let k = ref 0 in
  for i = 0 to n - 1 do
    res.indi.(i) <- !k;
    for j = 0 to m-1 do
      if mat.(i).(j) <> 0 then
	begin
	  res.v.(!k) <- mat.(i).(j);
	  res.j.(!k) <- j;
	  incr k;
	end
    done
  done;
  res

let to_dense mat =
  let res = Array.make_matrix mat.n mat.m 0 in
  iter (fun i j v -> res.(i).(j) <- v) mat;
  res

(* FIXME : planned to be removed *)
let to_sparseblock mat =
  let entries, iindices, jindices = ref [],  ref [], ref [] in
  let f i j v =
    if (i <= j) then
      begin
	entries := v :: !entries;
	iindices := i :: !iindices;
	jindices := j :: !jindices
      end in
  iter f mat;
  (* FIXME : unoptimized *)
  { Sdp.entries = Array.of_list (List.rev !entries);
    Sdp.iindices = Array.of_list (List.rev !iindices);
    Sdp.jindices = Array.of_list (List.rev !jindices) }
    
(*
let from_dense zero mat =
  let n = Array.length mat
  and m = Array.length mat.(0) in
  let nb = dense_count_values zero mat in
  let res = make n m nb zero in
  res.indi.(n) <- nb; (**)
  let k = ref 0 in
  for i = 0 to n - 1 do
    res.indi.(i) <- !k;
    for j = 0 to m-1 do
      if mat.(i).(j) <> zero then
	begin
	  res.v.(!k) <- mat.(i).(j);
	  res.j.(!k) <- j;
	  incr k;
	end
    done
  done;
  res

let to_dense zero mat =
  let res = Array.make_matrix mat.n mat.m zero in
  iter (fun i j v -> res.(i).(j) <- v) mat;
  res
    
let dense_mul (zero : 'a) (add : 'a -> 'a -> 'a) (mul : 'a -> 'a -> 'a) a b =
  let n = Array.length a
  and l = Array.length b
  and m = Array.length b.(0) in
  let res = Array.make_matrix n m zero in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      for k = 0 to l-1 do
	res.(i).(j) <- add res.(i).(j) (mul a.(i).(k) b.(k).(j))
      done
    done
  done;
  res

let dense_apply zero add mul mat vect =
  let f x = [|x|]
  and g bx = bx.(0) in
  Array.map g (dense_mul zero add mul mat (Array.map f vect))

let dense_transpose mat =
  let n = Array.length mat
  and m = Array.length mat.(0) in  
  let res = Array.make_matrix m n (-1) in
  for i = 0 to n-1 do
    for j = 0 to m-1 do
      res.(j).(i) <- mat.(i).(j)
    done
  done;
  res

let transpose mat =
  from_dense 0 (dense_transpose (to_dense 0 mat))
    
let _ = Random.self_init
  
let random n m =
  let res = Array.make_matrix n m 0 in
  for x = 0 to n*m/15 do
    let i = Random.int n in
    let j = Random.int m in
    res.(i).(j) <- Random.int 100;
  done;
  res
    
let a =
  [|
    [| 3; 4; 0; 4|];
    [| 2; 0; 0; 4|];
    [| 0; 0; 7; 0|] 
  |]

let b = 
  [|
    [| 0; 3 |];
    [| 1; 2 |];
    [| 10; 0 |];
    [| 100; 0 |] 
  |]


let a = random 70 50
let b = random 50 60


let y = dense_mul 0 (+) ( * ) a b

let a' = from_dense 0 a and b' = from_dense 0 b 


let y' = mul a' b'

let y'' = to_dense 0 y'
  
let tada = (y,y'')
let tindin = y = y''
  
(* let y = apply sm v
  let y' = dense_apply 0 (+) ( * ) m v
  *)

let dense_bilinear x m y =
  let op = dense_mul 0 ( + ) ( * ) in
  (op [| x |] (op m (dense_transpose [| y |]))).(0).(0)
  
let a = [|1;2;3;4|]
let b = [|10;30;40|]
  
let h = 
  [|
    [| 0; 3 ;2 |];
    [| 1; 0 ; 0|];
    [| 10; 0 ; 5|];
    [| 100; 0; 90 |] 
  |]

let h' = from_dense 0 h

let q = dense_bilinear a h b
let q' = bilinear_form a h' b

  *)
