open Defs

type t

val empty : t

val add : Building.t -> t -> t

val apply_manp : manpower -> t -> t
val deduce : supply -> t -> supply * t

val status_of : t -> (Building.t * Resource.t) list

val tick : t -> Building.t list * t
