type kind = Ballista | Berserker | Cavalry | Cyclops | Demon | Dervish | Harpy | Knight | Men | Merc | Orc | Ranger | Skeleton | Templar

module Set : Set.S with type elt = kind

type report = (kind * Defs.count) list
type sum_report = (Defs.count * Set.t)

val attacks : kind list

module Attr : sig
  type t = kind -> bool
  val can_barrage : t
  val can_build : t
  val can_heal : t
  val can_reflect : t
  val is_cavalry : t
  val is_holy : t
  val is_infantry : t
  val is_infectable : t
  val is_revivable : t
  val is_siege : t
end

module Base : sig
  val abundance : kind -> float
  val chance : kind -> Defs.chance
  val chance_growth : kind -> Defs.chance
  val supply_cost : kind -> Defs.supply
end

val translate : kind -> kind -> Defs.count -> Defs.count

type t

val empty : t

val make : Defs.count -> kind -> t
val cost : Defs.count -> kind -> t

val affordable : kind -> Defs.count -> t -> Defs.count
val barrage_power : t -> Defs.power
val count : kind -> t -> Defs.count
val count_all : t -> Defs.count
val dr : t -> Defs.power
val filter_count : Attr.t -> t -> Defs.count
val filter_power : Attr.t -> t -> Defs.power
val find : Defs.count -> kind -> t -> Defs.count
val has : kind -> t -> bool
val kinds_of : t -> Set.t
val power : t -> Defs.power
val power_of : kind -> t -> Defs.power
val promotable : kind -> t -> Defs.count
val report : t -> report
val untouchable : t -> t -> kind list
val upkeep : t -> Defs.supply

val add : Defs.count -> kind -> t -> t
val combine : t -> t -> t
val discard : Attr.t -> t -> t
val filter : Attr.t -> t -> t
val only : kind -> t -> t
val pop : kind -> t -> t * t
val reduce : t -> t -> t
val split : Attr.t -> t -> t * t
val starve : Defs.supply -> t -> t
val sub : Defs.count -> kind -> t -> t

module Dist : sig
  type result
  val empty : result
  val absorbed : result -> Defs.power
  val healed : result -> Defs.power
  val no_remaining : result -> bool
  val outcome : result -> t
  val reflected : result -> Defs.power
  val remaining : result -> t
  module Roll : Dice.S -> sig
    val from : Defs.power -> kind list -> t -> result
  end
end

module Fill : Dice.S -> sig
  val from : Defs.power -> t -> t
end

module FillCount : Dice.S -> sig
  val from : Defs.count -> t -> t
end

module Report : Dice.S -> sig
  val from : t -> report
  val sum_from : t -> sum_report
end
