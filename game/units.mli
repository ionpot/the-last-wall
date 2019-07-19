type kind = Ballista | Cavalry | Cyclops | Demon | Dervish | Harpy | Knight | Men | Orc | Ranger | Skeleton | Templar
type report = (kind * Defs.count) list
type sum_report = (Defs.count * kind list)

val attacks : kind list
val defends : kind list

val abundance_of : kind -> float
val chance_of : kind -> float
val chance_growth_of : kind -> float

type t

val empty : t

val make : Defs.count -> kind -> t

module Cost : sig
  val from : Defs.count -> kind -> t
  val supply : kind -> Defs.supply
end

module Dr : sig
  val cavalry : t -> Defs.power
  val harpy : t -> Defs.power
end

val affordable : kind -> Defs.count -> t -> Defs.count
val barrage_power : t -> Defs.power
val count : kind -> t -> Defs.count
val count_all : t -> Defs.count
val count_cavalry : t -> Defs.count
val count_holy : t -> Defs.count
val count_infantry : t -> Defs.count
val find : Defs.count -> kind -> t -> Defs.count
val has : kind -> t -> bool
val has_base_power : Defs.power -> t -> bool
val kinds_of : t -> kind list
val power : t -> Defs.power
val power_of : kind -> t -> Defs.power
val promotable : kind -> t -> Defs.count
val ratio : kind -> kind -> t -> float
val report : t -> report
val upkeep : t -> Defs.supply
val workforce : t -> Defs.power

val add : Defs.count -> kind -> t -> t
val combine : t -> t -> t
val reduce : t -> t -> t
val revivable : t -> t
val rm : kind -> t -> t
val starve : Defs.supply -> t -> t
val sub : Defs.count -> kind -> t -> t

module Dist : Dice.S -> sig
  val from : Defs.power -> t -> t
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
