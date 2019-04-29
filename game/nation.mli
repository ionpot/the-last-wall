type kind = Clan | Hekatium | Numendor | Sodistan | Tulron
type support = (kind * Resource.t) list
type trade = Boost of kind | Certain of kind | NoTrade
type t

val empty : t
val kinds : kind list
val max_allowed : int

val add : Resource.t -> support -> support
val sum : support -> Resource.t

val which : t -> kind list

val chosen : kind list -> t -> t

module Roll : Dice.S -> sig
  val support : trade -> t -> support
end
