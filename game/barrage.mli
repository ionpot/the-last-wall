type reason = Leader | Weather
type status = Available | Disabled of reason

type t

val empty : t

val is_chosen : t -> bool

val set_choice : bool -> t -> t
