type kind = Aristocrat | Expert | Warrior
type level = int
type charisma = int
type t

val empty : t
val kinds : kind list

val make : kind -> t
val random : unit -> t

val can_lvup : t -> bool
val can_respawn : Defs.turn -> t -> bool
val cha_of : t -> charisma
val defense_of : t -> float
val is_alive : t -> bool
val is_dead : t -> bool
val is_noble : t -> bool
val kind_of : t -> kind
val level_of : t -> level
val res_bonus_of : t -> Resource.t
val roll_death : t -> bool

val died : Defs.turn -> t -> t
val lvup : t -> t
val won : t -> t
