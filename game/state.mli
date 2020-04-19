type t

val empty : t

val deity : t -> Deity.t
val deity_set : Deity.t -> t -> t

val leader : t -> Leader.t
val leader_set : Leader.t -> t -> t
