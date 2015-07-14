type drawable = int -> int -> int -> unit

let draw_list_sizeable w h list =
  let n = List.length list in
  if n > 0 then
    let margin = 10 in
    let k =
      let b = (float_of_int w) /. (float_of_int (2 * h))
      and c = (float_of_int (w * n)) /. (float_of_int h) in
      let z = (b +. (sqrt (b*.b +. 4.*.c))) /. 2. in
      max (int_of_float z) 1
    in
    let l = (n + k - 1)/k in
    let step = (min (w/k) (h/l)) in
    let c = step - margin in
    let x i = margin + (i mod k)*step
    and y i = margin + (i/k)*step in
    Common.list_iteri (fun i f -> f (x i) (y i) c) list;; 

let draw_coeff s p =
  let f i j c =
    Graphics.moveto i (j+(c/2));
    Graphics.draw_string s;
    p (i+c/4) (j+c/8) ((c*3)/4) in
  f

let draw_list_drawable l =
  Graphics.open_graph " 1200x600";
  draw_list_sizeable 1200 600 l;
  let _ = Graphics.read_key () in
  Graphics.close_graph ()

let draw_list draw l =
  draw_list_drawable (List.map draw l)

let draw_sum print zero draw coef basis =
  let res = ref [] in
  for i = 0 to (Array.length basis) -1 do
    if coef.(i) <> zero then
      res := (draw_coeff (print coef.(i)) (draw basis.(i))) :: !res
  done;
  draw_list_drawable (List.rev !res)

let draw_alone draw g =
  Graphics.open_graph " 220x220";
  draw g 10 10 200;
  let _ = Graphics.read_key () in
  Graphics.close_graph ()
