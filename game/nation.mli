type kind = Clan | Hekatium | Numendor | Sodistan | Tulron
type support = (kind * Resource.t) list
type trade = Boost of kind | Certain of kind | NoTrade
type t

val empty : t
val kinds : kind list
val max_allowed : int

val add : Resource.t -> support -> support
val sum : support -> Resource.t

val trade : t -> trade
val which : t -> kind list

val boost : kind -> t -> t
val certain : kind -> t -> t
val chosen : kind list -> t -> t
val trading : bool -> t -> t

module Roll : Dice.S -> sig
  val support : t -> support
end
