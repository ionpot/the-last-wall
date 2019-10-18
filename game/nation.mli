open Defs

type kind = Clan | Hekatium | Numendor | Sodistan | Tulron

module Map : Map.S with type key = kind
module Set : Set.S with type elt = kind

val kinds : kind list
val max_allowed : int

val ranges_of : kind -> manpower range * supply range
val set2map : (Set.elt -> 'a) -> Set.t -> 'a Map.t

module Chance : sig
  type t
  val cap : chance
  val cap_trading : chance
  val add : chance -> kind -> t -> t
  val cap_at : chance -> kind -> t -> t
  val of_kind : kind -> t -> chance
  val set_trading : kind -> t -> t
  val sub : chance -> kind -> t -> t
  val sub_all : chance -> t -> t
end

type t

val empty : t

val chances : t -> Chance.t
val chosen : t -> Set.t
val has_aided : kind -> t -> bool

val map_chances : (Chance.t -> Chance.t) -> t -> t
val set_aided : Set.t -> t -> t
val set_chosen : Set.t -> t -> t
