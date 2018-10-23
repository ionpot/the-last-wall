type t
type ltype = Aristocrat | Expert | Warrior
type level = int
type charisma = int

val make : ltype -> t
val random : unit -> t
val lives : unit -> bool
val won : t -> unit
val can_lvup : t -> bool
val lvup : t -> t
val type_of : t -> ltype
val level_of : t -> level
val cha_of : t -> charisma
val defense_of : t -> float
val res_bonus_of : t -> Resource.t
