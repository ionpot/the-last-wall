open Defs

type kind = Clan | Hekatium | Numendor | Sodistan | Tulron
type trade = Boost of kind | Certain of kind | NoTrade

val kinds : kind list
val max_allowed : int

val ranges_of : kind -> manpower range * supply range

module Chance : sig
  type t
  val increase : kind -> t -> t
  val of_kind : kind -> t -> float
  val reduce : kind -> t -> t
end

type t

val empty : t

val chances : t -> Chance.t
val which : t -> kind list

val chosen : kind list -> t -> t
val map_chances : (Chance.t -> Chance.t) -> t -> t
