type reason = Leader | Weather
type status = Available | Disabled of reason

type t

val empty : t

val is_chosen : t -> bool
val is_trained : t -> bool

val set_choice : bool -> t -> t
val set_trained : bool -> t -> t
