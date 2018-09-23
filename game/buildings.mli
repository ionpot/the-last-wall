type t

val make : unit -> t

val build : Building.t list -> t -> unit

val count_of : Building.t -> t -> int
val is_ready : Building.t -> t -> bool

val draw_manp : Resource.t -> t -> Resource.t
val draw_supp : Resource.t -> t -> Resource.t

val tick : t -> unit
