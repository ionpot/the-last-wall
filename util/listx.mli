val build : ('a list -> 'a -> 'a list) -> 'a list -> 'a list
val count : 'a -> 'a list -> int
val discard : ('a -> bool) -> 'a list -> 'a list
val pick_first : int -> 'a list -> 'a list
val pick_from : 'a list -> 'a
val undupe : 'a list -> 'a list
