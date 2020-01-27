type reason = Archers | Leader | Weather
type status = Available | Disabled of reason

val base_coefficient : Defs.power
val trained_coefficient : Defs.power

type t

val empty : t

val coefficient : t -> Defs.power
val can_barrage : t -> bool
val can_hit_run : t -> bool

val set_choice : bool -> t -> t
val set_status : status -> t -> t
val set_trained : bool -> t -> t
