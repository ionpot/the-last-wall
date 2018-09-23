type t

val make : Building_status.t -> t

val is_built : t -> bool

val start : t -> unit
val fin : t -> unit

val add_manp : Resource.t -> t -> Resource.t
val add_supp : Resource.t -> t -> Resource.t
