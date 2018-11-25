open Defs

type t

val empty : t

val add : Building.t -> t -> t

val manp_cost : t -> manpower
val supp_cost : t -> supply

val set_manp : manpower -> t -> t
val set_supp : supply -> t -> t

val status_of : t -> (Building.t * Resource.t) list

val tick : t -> Building.t list * t
