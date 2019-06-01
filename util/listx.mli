val apply_to : ('a -> unit) list -> 'a -> unit
val build : ('a list -> 'a -> 'a list) -> 'a list -> 'a list
val compare : 'a list -> 'a -> 'a -> int
val count : 'a -> 'a list -> int
val dedupe : 'a list -> 'a list
val dedupe_if : ('a -> bool) -> 'a list -> 'a list
val discard : ('a -> bool) -> 'a list -> 'a list
val filteri : (int -> 'a -> bool) -> 'a list -> 'a list
val fold_map : ('a -> 'b -> 'a * 'b) -> 'a -> 'b list -> 'a * 'b list
val group : 'a list -> (int * 'a) list
val index_of : 'a -> 'a list -> int
val in_both : 'a list -> 'a list -> 'a list
val map_with : ('a -> 'b -> 'a * 'b) -> 'a -> 'b list -> 'b list
val min_of : int list -> int
val pick : float -> float list -> 'a list -> 'a
val pick_first : int -> 'a list -> 'a list
val rm : 'a -> 'a list -> 'a list
val rm_from : 'a list -> 'a list -> 'a list
val slice_from : ('a -> bool) -> 'a list -> 'a list
val sum : int list -> int
val sumf : float list -> float
val swap_nth : int -> 'a -> 'a list -> 'a list
val unfold : 'a -> ('a -> ('a * 'b) option) -> 'b list
val unfold_with : 'a -> ('a -> ('a * 'b) option) -> 'a * 'b list
