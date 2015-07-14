(** Shared printing functions. *)

let grey = "\027[1;30m"
and red = "\027[1;31m"
and white = "\027[0;0m"
and green = "\027[1;32m"
and yellow = "\027[1;33m"
and blue = "\027[1;34m"

let print c s =
  Printf.printf "%s%s%s" c s white;
  flush stdout

let p ?(color=green) s =
  if !Param.loquacity >= Param.Usual then print color s

let verbose ?(color=grey) s =
  if !Param.loquacity >= Param.Verbose then print color s

let debug ?(color=grey) s =
  if !Param.loquacity = Param.Debug then print color s
