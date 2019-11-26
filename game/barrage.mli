type reason = Leader | Target | Weather
type status = Available | Disabled of reason

val base_coefficient : Defs.power

type t

val empty : t

val coefficient : Bonus.t -> t -> Defs.power
val can_hit_run : t -> bool
val is_available : t -> bool
val is_chosen : t -> bool
val is_trained : t -> bool

val set_choice : bool -> t -> t
val set_status : status -> t -> t
val set_trained : bool -> t -> t
