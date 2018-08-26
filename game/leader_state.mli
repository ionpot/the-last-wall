type t

val empty : t
val dead : t

val get : t -> Leader.t option
val make : Leader.t -> t
val need : t -> bool
val tick : t -> t
