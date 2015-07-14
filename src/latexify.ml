(** Pretty-printing of small sdp problems in latex format. *)
open Sdp

let pf = Printf.fprintf

let header out =
  pf out "\\documentclass{article}\n";
  pf out "\\usepackage{fullpage}\n";
  pf out "\\usepackage[T1]{fontenc}\n";
  pf out "\\usepackage[utf8]{inputenc}\n";
  pf out "\\usepackage{amsmath}\n";
  pf out "\\begin{document}\n";
  pf out "\\setcounter{MaxMatrixCols}{100}\n"

let tailer out =
  pf out "\\end{document}\n"

(* sdp problems *)
let block_offset types =
  let n = Array.length types in
  let offset = Array.make (n+1) 0 in
  for i = 0 to n-1 do
    offset.(i+1) <- offset.(i) + (block_size types.(i))
  done;
  offset

let tab_offset out n =
  for i = 1 to n do
    pf out " & "
  done

let print_sparseblock out zero print_elem btype offset total_offset b =
  let n = block_size btype in
  let b_string = sparseblock_map print_elem b in
  let m = matrix_of_sparseblock zero n b_string in
  for i = 0 to n-1 do
    tab_offset out offset;
    for j = 0 to n-1 do
      let s =
	if (match btype with MAT _ -> true | DIAG _ -> i=j)
	then m.(i).(j)
	else "" in
      pf out " %s & " s
    done;
    tab_offset out (total_offset - offset - n);
    pf out "\\\\\n"
  done

let print_matrix out zero print_elem types m =
  let n = Array.length m in
  let offsets = block_offset types in
  pf out "\\begin{pmatrix}\n";
  for i = 0 to n-1 do
    print_sparseblock out zero print_elem
      types.(i) offsets.(i) offsets.(n) m.(i)
  done;
  pf out "\\end{pmatrix}\n"

let print_sdp_pb out c ctype a b =
  let my_print_matrix m = print_matrix out "0" (Printf.sprintf "%.2f") ctype m in
  pf out "$$C=\n";
  my_print_matrix c;
  pf out "$$\n";
  for i = 0 to (Array.length b) - 1 do
    pf out "$$A_{%d}=\n" (i+1);
    my_print_matrix a.(i);
    pf out ",\nb_{%d}=%.2f$$\n" (i+1) b.(i)
  done

let pb_to_file file (ctype, c, a, b) =
  let out = open_out file in
  header out;
  print_sdp_pb out c ctype a b;
  tailer out;
  close_out out
