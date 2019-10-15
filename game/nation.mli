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
  val base : chance
  val add : chance -> kind -> t -> t
  val cap_at : chance -> kind -> t -> t
  val of_kind : kind -> t -> chance
  val sub : chance -> kind -> t -> t
  val sub_all : chance -> t -> t
end

type t

val empty : t

val chances : t -> Chance.t
val chosen : t -> Set.t

val map_chances : (Chance.t -> Chance.t) -> t -> t
val set_chosen : Set.t -> t -> t
