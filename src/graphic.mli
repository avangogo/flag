(** Drawing graphs with the Graphics package. *)
type drawable = int -> int -> int -> unit
val draw_list : ('a -> drawable) -> 'a list -> unit
val draw_sum :
  ('a -> string) -> 'a -> ('b -> drawable) -> 'a array -> 'b array -> unit
val draw_alone : ('a -> drawable) -> 'a -> unit
