val apply_to : ('a -> unit) list -> 'a -> unit
val build : ('a list -> 'a -> 'a list) -> 'a list -> 'a list
val count : 'a -> 'a list -> int
val discard : ('a -> bool) -> 'a list -> 'a list
val min_of : int list -> int
val pick_first : int -> 'a list -> 'a list
val pick_from : 'a list -> 'a
val rm : 'a -> 'a list -> 'a list
val slice_from : ('a -> bool) -> 'a list -> 'a list
val undupe : 'a list -> 'a list
