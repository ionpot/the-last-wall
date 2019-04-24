type kind = Clan | Hekatium | Numendor | Sodistan | Tulron
type support = (kind * Resource.t) list
type t

val empty : t
val kinds : kind list
val max_allowed : int

val add : Resource.t -> support -> support
val sum : support -> Resource.t

val which : t -> kind list

val boost : kind -> t -> t
val certain : kind -> t -> t
val chosen : kind list -> t -> t

module Roll : Dice.S -> sig
  val support : t -> support
end
