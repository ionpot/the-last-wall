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
  val increase_by : float -> kind -> t -> t
  val of_kind : kind -> t -> float
  val reduce_by : float -> kind -> t -> t
end

type t

val empty : t

val chances : t -> Chance.t
val chosen : t -> Set.t

val map_chances : (Chance.t -> Chance.t) -> t -> t
val set_chosen : Set.t -> t -> t
