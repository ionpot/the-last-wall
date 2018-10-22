open Defs

type t

val empty : t

val add : Building.t -> t -> t

val add_manp : manpower -> t -> manpower * t
val add_supp : supply -> t -> supply * t

val status_of : t -> (Building.t * Resource.t) list

val tick : t -> Building.t list * t
