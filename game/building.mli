type t = Market | Stable | Tavern | Temple

val ready : t list
val tlist : t list

val cost_of : t -> Resource.t
val multiple : t -> bool
