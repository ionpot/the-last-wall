type charisma = Defs.count
type gender = Female | Male
type kind = Aristocrat | Expert | Warrior
type level = Defs.count
type t

val empty : t
val kinds : kind list

val can_respawn : Defs.turn -> t -> bool
val cha_of : t -> charisma
val defense_of : t -> Defs.power
val gender_of : t -> gender
val is_alive : t -> bool
val is_dead : t -> bool
val is_noble : t -> bool
val kind_of : t -> kind
val level_of : t -> level
val lvup : t -> bool
val res_bonus_of : t -> Resource.t
val victories : t -> Defs.count

val died : Defs.turn -> t -> t
val won : t -> t

module Roll : Dice.S -> sig
  val death : t -> bool
  val from : kind -> t
  val random : unit -> t
end
