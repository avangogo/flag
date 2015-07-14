type t = int * int

let nom (n, _) = n

let denom (_, d) = d

let rec gcd a b =
  let r = a mod b in
  if r = 0 then b
  else gcd b r

let simplify (n, d) =
  let g = gcd n d in
  let r = if g*d > 0 then g else -g in
  (n/r, d/r)

let zero = (0, 1)

let make n d =
  if d == 0 then failwith "Rational.make : division by 0";
  simplify (n, d)

let int n = (n, 1)

let rat r = r

let mul (n, d) (n', d') =
  simplify (n*n', d*d')

let div (n, d) (n', d') =
  simplify (n*d', d*n')

let add (n, d) (n', d') =
  simplify (n*d' + n'*d, d*d')

let sub (n, d) (n', d') =
  simplify (n*d' - n'*d, d*d')

let minus (n, d) =
  (-n, d)

let compare (n, d) (n', d') =
  n * d' - n' * d

let leq a b =
  compare a b <= 0

let to_float (n, d) =
  (float_of_int n) /. (float_of_int d)

let to_int (n, d) =
  if d = 1 then n
  else failwith "Rational.to_int : invalid argument"

let print (n, d) =
  if d = 1 then Printf.sprintf "%d" n
  else Printf.sprintf "%d/%d" n d
