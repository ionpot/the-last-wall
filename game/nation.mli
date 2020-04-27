open Defs

type kind = Clan | Hekatium | Numendor | Sodistan | Tulron

module Map : Map.S with type key = kind
module Set : Set.S with type elt = kind

type chances = Defs.percent Map.t
type resources = Resource.t Map.t

val kinds : kind list
val max_allowed : int

type t

val empty : t

val barracks : t -> kind option
val barracks_set : kind option -> t -> t
val chances : t -> chances
val chances_map : (chances -> chances) -> t -> t
val chances_set : chances -> t -> t
val chosen : t -> Set.t
val chosen_set : Set.t -> t -> t
val has_aided : kind -> t -> bool
val has_barracks : kind -> t -> bool
val has_trade : kind -> t -> bool
val has_traded : kind -> t -> bool
val no_barracks : t -> bool
val no_trade : t -> bool
val mnp_from : kind -> t -> manpower
val sup_from : kind -> t -> supply
val support : t -> resources
val support_set : resources -> t -> t
val trade : t -> kind option
val trade_set : kind option -> t -> t
val traded_mnp : kind -> t -> manpower
val traded_sup : kind -> t -> supply
