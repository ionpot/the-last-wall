type kind = Ballista | Berserker | Cavalry | Cyclops | Demon | Dervish | Dullahan | Harcher | Harpy | Knight | Mangonel | Marms | Men | Merc | Novice | Orc | Ranger | Skeleton | Templar | Veteran | Xbowman

module Map : Map.S with type key = kind
module Set : Set.S with type elt = kind

type report = (kind * Defs.count) list
type sum_report = (Defs.count * Set.t)

val attacks : kind list
val starve_order : kind list

module Attr : sig
  type t
  val archer : t
  val barrage : t
  val barraged : t
  val build : t
  val cavalry : t
  val fear : t
  val flying : t
  val heal : t
  val hit_run : t
  val holy : t
  val infantry : t
  val infectable : t
  val reflect : t
  val revivable : t
  val siege : t
  val undead : t
  val fold : t -> (kind -> 'a -> 'a) -> 'a -> 'a
  val is : t -> kind -> bool
  val set_of : t -> Set.t
end

module Base : sig
  val abundance : kind -> float
  val artillery : kind -> Defs.power
  val chance : kind -> Defs.chance
  val chance_growth : kind -> Defs.chance
  val dr : kind -> Defs.power
  val hit_chance : kind -> Defs.power
  val power : kind -> Defs.power
  val revive : kind -> Defs.power
  val supply_cost : kind -> Defs.supply
end

type t = Defs.count Map.t

val empty : t

val make : Defs.count -> kind -> t

module Promote : sig
  val affordable : kind -> Defs.count -> t -> Defs.count
  val amount : kind -> Defs.count
  val cost : Defs.count -> kind -> t
  val max : kind -> t -> Defs.count
  val needs : kind -> kind
end

val count : kind -> t -> Defs.count
val count_all : t -> Defs.count
val filter_count : Attr.t -> t -> Defs.count
val find : Defs.count -> kind -> t -> Defs.count
val has : kind -> t -> bool
val has_any : Attr.t -> t -> bool
val is_empty : t -> bool
val kinds_of : t -> Set.t
val power_of : t -> Defs.power
val ratio_of : kind -> t -> float
val report : t -> report
val upkeep : t -> Defs.supply

val add : Defs.count -> kind -> t -> t
val combine : t -> t -> t
val discard : Attr.t -> t -> t
val filter : Attr.t -> t -> t
val filterset : Set.t -> t -> t
val only : kind -> t -> t
val pop : kind -> t -> t * t
val reduce : t -> t -> t
val split : Attr.t -> t -> t * t
val starve : Defs.supply -> t -> t
val sub : Defs.count -> kind -> t -> t

module Roll : sig
  val fill : Defs.count -> t -> t * t
  val report : t -> report
  val sum_report : t -> sum_report
end
