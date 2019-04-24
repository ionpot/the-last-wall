type kind = Tulron | Sodistan | Hekatium | Numendor | Clan
type support = (kind * Resource.t) list
type t

val empty : t
val kinds : kind list
val max_allowed : int

val from : kind list -> t

val add : Resource.t -> support -> support
val sum : support -> Resource.t
val which : t -> kind list

module Roll : Dice.S -> sig
  val support : t -> support
end
