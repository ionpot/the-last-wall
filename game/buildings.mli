open Defs

type t
type queued = Building.t * Resource.t
type status = Building.t list * Building.t list * queued list

val empty : t

val build : Building.t list -> t -> t
val raze : Building.t -> t -> t

val count_of : Building.t -> t -> int
val is_ignored : Building.t -> t -> bool
val is_ready : Building.t -> t -> bool

val manp_cost : t -> manpower
val supp_cost : t -> supply

val apply_manp : manpower -> manpower -> t -> t
val deduce : supply -> supply -> t -> supply * t

val to_status : t -> status
val update : status -> t -> t
