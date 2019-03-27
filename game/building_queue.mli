open Defs

type t
type ongoing = (Building.t * Resource.t) list
type status = Building.t list * ongoing

val empty : t

val add : Building.t -> t -> t

val manp_cost : t -> manpower
val supp_cost : t -> supply

val set_manp : manpower -> t -> t
val set_supp : supply -> t -> t

val status_of : t -> status
val update : ongoing -> t -> t
