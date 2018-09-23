type t

val make : Building.t -> t

val cost_of : t -> Resource.t
val count_of : t -> int

val can_start : t -> bool
val is : Building.t -> t -> bool
val is_ready : t -> bool

val start : t -> unit
val fin : t -> unit
