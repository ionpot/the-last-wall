type t

val empty : t

val build : t -> Build.t
val build_map : (Build.t -> Build.t) -> t -> t

val deity : t -> Deity.t
val deity_set : Deity.t -> t -> t

val leader : t -> Leader.t
val leader_set : Leader.t -> t -> t

val month_set : Month.t -> t -> t

val nation : t -> Nation.t
val nation_map : (Nation.t -> Nation.t) -> t -> t

val resource_set : Resource.t -> t -> t

val turn : t -> Defs.turn

val units_set : Units.t -> t -> t
