type reason = Leader | Weather
type status = Available | Disabled of reason

val base_coefficient : Defs.power

type t

val empty : t

val coefficient : t -> Defs.power
val is_chosen : t -> bool
val is_trained : t -> bool

val set_choice : bool -> t -> t
val set_trained : bool -> t -> t
