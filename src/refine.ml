(*nsets elems loc sidx begin end mid*)
type t = int ref * int array * int array * int array * int array * int array * int array
type elt = int
type set = int

let make n : t =
  let set = ref 1
  and elems = Array.init n (fun i -> i) 
  and loc = Array.init n (fun i -> i)
  and sidx = Array.make n 0
  and first = Array.make n (-1)
  and end_ = Array.make n (-1)
  and mid =  Array.make n (-1) in
  first.(0) <- 0;
  end_.(0) <- n;
  mid.(0) <- 0;
  set, elems, loc, sidx, first, end_, mid

let size ((_, _, _, _, first, end_ ,_ ) :t ) s = end_.(s) - first.(s)

let set ((_, _, _, sidx, _, _, _) :t ) e = sidx.(e)

let first ((_, elems, _, _, first, _, _) :t ) s = elems.(first.(s))

let total_size (_, elems, _, _, _, _, _) =
  Array.length elems

let next ((_, elems, loc, sidx, _, end_, _) :t ) e =
  let i = loc.(e) + 1 in
  if i >= end_.(sidx.(e)) then failwith "next"
  else elems.(i)

let mark ((_, elems, loc, sidx, _, _, mid) :t ) e =
  let s = sidx.(e)
  and l = loc.(e) in
  let m = mid.(s) in
  if l >= m then
    begin
      elems.(l) <- elems.(m);
      loc.(elems.(l)) <- l;
      elems.(m) <- e;
      loc.(e) <- m;
      mid.(s) <- m + 1
    end

let split ((sets, elems, _, sidx, first, end_, mid) :t ) s =
  if mid.(s) = end_.(s) then mid.(s) <- first.(s);
  if mid.(s) = first.(s) then (-1)
  else
    begin
      let set = !sets in
      incr sets;
      first.(set) <- first.(s);
      mid.(set)  <- first.(s);
      end_.(set) <- mid.(s);
      first.(s) <- mid.(s);
      for i = first.(set) to end_.(set) - 1 do
	sidx.(elems.(i)) <- set
      done;
      set
    end

let no_marks ((_, _, _, _, first, _, mid) :t ) s =
  mid.(s) = first.(s)

let iter ((_, elems, _, _, first, end_, _) :t ) f s =
  for i = first.(s) to end_.(s) - 1 do
    f elems.(i)
  done

let sets ((sets, _, _, _, _, _, _) :t ) =
  !sets

let cp = Array.copy

let copy ((sets, elems, loc, sidx, first, end_, mid) :t ) =
  (ref !sets, cp elems, cp loc, cp sidx, cp first, cp end_, cp mid) 

let cut t f els =
  List.iter (mark t) els;
  let touched = List.sort compare (List.rev_map (set t) els) in
  List.iter (fun s -> let new_s = split t s in if new_s <> -1 then f new_s) touched

let make_with_fixed_begin n k_ =
  assert (k_ <= n);
  let k = min k_ (n - 1) in (* In the case k = n, everithing will be as if k = n-1 *)
  let sets, elems, loc, sidx, first, end_, mid as t = make n in
  sets := k+1;
  for i = 1 to k do
    sidx.(i-1) <- i-1;
    end_.(i-1) <- i;
    first.(i) <- i;
    mid.(i) <- i
  done;
  end_.(k) <- n;
  for i = k to n-1 do
    sidx.(i) <- k
  done;
  t
