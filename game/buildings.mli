type t
type queued = (Building.t * Resource.t)

val make : unit -> t

val build : Building.t list -> t -> unit

val count_of : Building.t -> t -> int
val is_ready : Building.t -> t -> bool

val draw_manp : Resource.t -> t -> Resource.t
val draw_supp : Resource.t -> t -> Resource.t

val in_queue : t -> queued list
val tick : t -> Building.t list
