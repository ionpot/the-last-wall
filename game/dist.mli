type t

val empty : t

val absorbed : t -> Defs.power
val healed : t -> Defs.power
val no_remaining : t -> bool
val outcome : t -> Units.t
val reflected : t -> Defs.power
val remaining : t -> Units.t
val retreated : t -> Units.t

module type Flags = sig
  val full_absorb : bool
  val use_ratio : bool
end

module Damage : Dice.S -> Flags -> sig
  val from : Defs.power -> Power.t -> Units.t -> Units.t -> t
end

module Fill : Dice.S -> sig
  val from : Defs.power -> Power.t -> Units.t -> Units.t * Units.t
end

val fill : base:Power.t -> order:Units.kind list -> Defs.power -> Units.t -> Defs.power * Units.t
