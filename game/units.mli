type kind = Ballista | Berserker | Cavalry | Cyclops | Demon | Dervish | Harpy | Knight | Men | Merc | Orc | Ranger | Skeleton | Templar
type report = (kind * Defs.count) list
type sum_report = (Defs.count * kind list)

val attacks : kind list

module Attr : sig
  val can_barrage : kind -> bool
  val can_build : kind -> bool
  val can_heal : kind -> bool
  val can_reflect : kind -> bool
  val is_cavalry : kind -> bool
  val is_holy : kind -> bool
  val is_infantry : kind -> bool
  val is_revivable : kind -> bool
  val is_siege : kind -> bool
end

module Base : sig
  val abundance : kind -> float
  val chance : kind -> float
  val chance_growth : kind -> float
  val supply_cost : kind -> Defs.supply
end

val translate : kind -> kind -> Defs.count -> Defs.count

type t

val empty : t

val make : Defs.count -> kind -> t
val cost : Defs.count -> kind -> t

val affordable : kind -> Defs.count -> t -> Defs.count
val count : kind -> t -> Defs.count
val count_all : t -> Defs.count
val dr : t -> Defs.power
val filter_count : (kind -> bool) -> t -> Defs.count
val filter_power : (kind -> bool) -> t -> Defs.power
val find : Defs.count -> kind -> t -> Defs.count
val has : kind -> t -> bool
val kinds_of : t -> kind list
val power : t -> Defs.power
val power_of : kind -> t -> Defs.power
val promotable : kind -> t -> Defs.count
val report : t -> report
val untouchable : t -> t -> kind list
val upkeep : t -> Defs.supply

val add : Defs.count -> kind -> t -> t
val combine : t -> t -> t
val discard : (kind -> bool) -> t -> t
val filter : (kind -> bool) -> t -> t
val only : kind -> t -> t
val reduce : t -> t -> t
val split : kind -> t -> t * t
val starve : Defs.supply -> t -> t
val sub : Defs.count -> kind -> t -> t

module Dist : sig
  type result
  val empty : result
  val absorbed : result -> Defs.power
  val healed : result -> Defs.power
  val move_back : kind -> result -> result
  val no_remaining : result -> bool
  val outcome : result -> t
  val reflected : result -> Defs.power
  val remaining : result -> t
  module Roll : Dice.S -> sig
    val from : Defs.power -> t -> result
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
