type t

val make : Building.t -> t

val cost_of : t -> Resource.t
val count_of : t -> int
val which : t -> Building.t

val can_start : t -> bool
val is : Building.t -> t -> bool
val is_built : t -> bool
val is_ready : t -> bool

val add_manp : Resource.t -> t -> Resource.t
val add_supp : Resource.t -> t -> Resource.t

val start : t -> unit
val tick : t -> unit
