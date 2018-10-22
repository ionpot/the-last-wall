open Defs

type t
type queued = (Building.t * Resource.t)

val make : unit -> t

val build : Building.t list -> t -> t

val count_of : Building.t -> t -> int
val is_ready : Building.t -> t -> bool

val in_queue : t -> queued list
val built : t -> Building.t list

val add_manp : manpower -> t -> manpower * t
val add_supp : supply -> t -> supply * t

val tick : t -> t
