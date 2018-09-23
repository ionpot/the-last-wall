type t = Market | Stable | Tavern | Temple

val tlist : t list

val built : t -> bool
val cost_of : t -> Resource.t
val multiple : t -> bool
