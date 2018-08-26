type t

val empty : t
val dead : t

val leader_of : t -> Leader.t option
val of_leader : Leader.t -> t
val need : t -> bool
val tick : t -> t
