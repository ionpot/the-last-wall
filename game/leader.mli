type t
type ltype = Aristocrat | Expert | Warrior
type level = int
type charisma = int

val empty : t
val ltypes : ltype list

val make : ltype -> t
val random : unit -> t

val can_lvup : t -> bool
val can_respawn : Defs.turn -> t -> bool
val cha_of : t -> charisma
val defense_of : t -> float
val has_died : t -> bool
val is_alive : t -> bool
val is_dead : t -> bool
val level_of : t -> level
val res_bonus_of : t -> Resource.t
val type_of : t -> ltype

val died : Defs.turn -> t -> t
val lvup : t -> t
val won : t -> t
