open Defs

type kind = Clan | Hekatium | Numendor | Sodistan | Tulron

module Map : Map.S with type key = kind
module Set : Set.S with type elt = kind

type support = Resource.t Map.t

val kinds : kind list
val max_allowed : int

val ranges_of : kind -> manpower range * supply range
val set2map : (Set.elt -> 'a) -> Set.t -> 'a Map.t

module Chance : sig
  type t
  val cap : percent
  val cap_trading : percent
  val add : percent -> kind -> t -> t
  val cap_at : percent -> kind -> t -> t
  val of_kind : kind -> t -> percent
  val set_trading : kind -> t -> t
  val sub : percent -> kind -> t -> t
  val sub_all : percent -> t -> t
end

type t

val empty : t

val barracks : t -> kind option
val cap_of : kind -> t -> percent
val chances : t -> Chance.t
val chosen : t -> Set.t
val has_aided : kind -> t -> bool
val has_barracks : kind -> t -> bool
val has_trade : kind -> t -> bool
val has_traded : kind -> t -> bool
val no_barracks : t -> bool
val no_trade : t -> bool
val mnp_from : kind -> t -> manpower
val sup_from : kind -> t -> supply
val trade : t -> kind option
val traded_mnp : kind -> t -> manpower
val traded_sup : kind -> t -> supply

val map_chances : (Chance.t -> Chance.t) -> t -> t
val set_barracks : kind option -> t -> t
val set_chosen : Set.t -> t -> t
val set_support : support -> t -> t
val set_trade : kind option -> t -> t
val trim_chances : t -> t
