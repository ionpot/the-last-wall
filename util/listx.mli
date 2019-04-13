val apply_to : ('a -> unit) list -> 'a -> unit
val build : ('a list -> 'a -> 'a list) -> 'a list -> 'a list
val count : 'a -> 'a list -> int
val discard : ('a -> bool) -> 'a list -> 'a list
val fold_map : ('a -> 'b -> 'a * 'b) -> 'a -> 'b list -> 'a * 'b list
val map_with : ('a -> 'b -> 'a * 'b) -> 'a -> 'b list -> 'b list
val min_of : int list -> int
val pick_first : int -> 'a list -> 'a list
val pick_from : 'a list -> 'a
val rm : 'a -> 'a list -> 'a list
val slice_from : ('a -> bool) -> 'a list -> 'a list
val sumf : float list -> float
val swap_nth : int -> 'a -> 'a list -> 'a list
val undupe : 'a list -> 'a list
val unfold : 'a -> ('a -> ('a * 'b) option) -> 'b list
val unfold_with : 'a -> ('a -> ('a * 'b) option) -> 'a * 'b list
