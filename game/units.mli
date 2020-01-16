type kind = Ballista | Berserker | Cavalry | Cyclops | Demon | Dervish | Dullahan | Harcher | Harpy | Knight | Men | Merc | Novice | Orc | Ranger | Skeleton | Templar | Veteran

module Map : Map.S with type key = kind
module Set : Set.S with type elt = kind

type report = (kind * Defs.count) list
type sum_report = (Defs.count * Set.t)

val attacks : kind list
val starve_order : kind list

module Attr : sig
  type t = kind -> bool
  val can_barrage : t
  val can_barraged : t
  val can_build : t
  val can_fear : t
  val can_heal : t
  val can_hit_run : t
  val can_reflect : t
  val is_cavalry : t
  val is_holy : t
  val is_infantry : t
  val is_infectable : t
  val is_revivable : t
  val is_siege : t
  val is_trained : t
  val is_undead : t
end

module Base : sig
  val abundance : kind -> float
  val chance : kind -> Defs.chance
  val chance_growth : kind -> Defs.chance
  val dr : kind -> Defs.power
  val hit_chance : kind -> Defs.power
  val power : kind -> Defs.power
  val revive : kind -> Defs.power
  val supply_cost : kind -> Defs.supply
end

module Bonus : sig
  type value = float
  type t
  val empty : t
  val attr : Attr.t -> value -> t -> t
  val kind : kind -> value -> t -> t
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
val ratio_of : kind -> t -> float
val report : t -> report
val upkeep : Bonus.t -> t -> Defs.supply

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

module Fill : Dice.S -> sig
  val from : Defs.count -> t -> t * t
end

module Report : Dice.S -> sig
  val from : t -> report
  val sum_from : t -> sum_report
end
