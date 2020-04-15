type charisma = Defs.count
type gender = Female | Male
type kind = Aristocrat | Engineer | Merchant
type level = Defs.count
type t

val empty : t
val kinds : kind list

val can_level_up : t -> bool
val can_respawn : Defs.turn -> t -> bool
val cha_mod_of : t -> charisma
val cha_of : t -> charisma
val defense_of : t -> Defs.power
val gender_of : t -> gender
val is : kind -> t -> bool
val is_alive : t -> bool
val is_dead : t -> bool
val is_living : kind -> t -> bool
val is_noble : t -> bool
val kind_of : t -> kind
val level_of : t -> level
val name_of : t -> Name.t
val victories : t -> Defs.count

val died : Defs.count -> Defs.turn -> t -> t
val level_up : t -> t
val won : t -> t

module Roll : sig
  val death : t -> bool
  val from : kind -> t
  val pair : unit -> t * t
end
