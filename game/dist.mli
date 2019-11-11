type t

val empty : t

val absorbed : t -> Defs.power
val healed : t -> Defs.power
val no_remaining : t -> bool
val outcome : t -> Units.t
val reflected : t -> Defs.power
val remaining : t -> Units.t

module Damage : Dice.S -> sig
  val absorb : Defs.power -> Power.t -> Units.t -> Units.t -> t
  val from : Defs.power -> Power.t -> Units.t -> Units.t -> t
end

module Fill : Dice.S -> sig
  val from : Defs.power -> Power.t -> Units.t -> Units.t * Units.t
end
