type t
type ltype = Aristocrat | Expert | Warrior
type level = int
type loss = Resource.t
type charisma = int

val make : unit -> t
val lives : unit -> bool
val won : t -> unit
val can_lvup : t -> bool
val lvup : t -> t
val type_of : t -> ltype
val level_of : t -> level
val cha_of : t -> charisma
val mitigate : loss -> t -> loss
val res_bonus_of : t -> Resource.t
