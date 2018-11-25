open Defs

type t
type queued = (Building.t * Resource.t)

val empty : t

val build : Building.t list -> t -> t

val count_of : Building.t -> t -> int
val is_ready : Building.t -> t -> bool

val in_queue : t -> queued list
val built : t -> Building.t list

val manp_cost : t -> manpower
val supp_cost : t -> supply

val apply_manp : manpower -> manpower -> t -> t
val deduce : supply -> supply -> t -> supply * t

val tick : t -> t
