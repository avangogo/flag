type expr =
  | Plus of expr * expr
  | Minus of expr
  | Times of expr * expr
  | Unlab of expr
  | Flag of string
  | Num of string
  | Zero
  | One

let rec simplify0 = function
  | Plus (a, Zero) -> a
  | Plus (Zero, a) -> a
  | Times (Zero, a) -> Zero
  | Times (a, Zero) -> Zero
  | Times (One, a) -> a
  | Times (a, One) -> a
  | Minus Zero -> Zero
  | Minus (Minus a) -> a
  | a -> a

let rec simplify = function 
  | Plus (a, b) -> simplify0 (Plus (simplify a, simplify b))
  | Times (a, b) -> simplify0 (Times (simplify a, simplify b))
  | Minus a -> simplify0 (Minus (simplify a))
  | Unlab a -> simplify0 (Unlab (simplify a))
  | a -> simplify0 a
      
let is_sum = function
  | Plus _ -> true
  | _ -> false

let box a s =
  if (is_sum a) then Printf.sprintf "(%s)" s else s
	   
let rec print0 = function
  | Plus (a, (Minus b)) -> Printf.sprintf "%s - %s" (print0 a) (print0 b)
  | Plus (a, b) -> Printf.sprintf "%s + %s" (print0 a) (print0 b) 
  | Minus a -> Printf.sprintf "-%s" (box a (print0 a))
  | Times (Num a, b) ->
     Printf.sprintf "%s.%s" a (box b (print0 b))
  | Times (a, b) when a = b ->
     Printf.sprintf "%s^2" (box a (print0 a)) 
  | Times (a, b) ->
     Printf.sprintf "%s*%s" (box a (print0 a)) (box b (print0 b))  
  | Unlab a -> Printf.sprintf "[| %s |]" (print0 a)
  | Flag s -> s 
  | Num s -> s
  | Zero -> "0"
  | One -> "1"

let print a =
  print0 (simplify a)

let expr print int x =
  if x = int 0 then Zero
  else if x = int 1 then One
  else if x = int (-1) then Minus One
  else Num (print x)
