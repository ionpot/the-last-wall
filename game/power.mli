module Map = Units.Map

type t = Defs.power Map.t

val base : t

val artillery : Units.t -> t
val dr : Units.t -> t
val revive : Units.t -> t

module Fn : sig
  val count : t -> Units.kind -> Defs.power -> Defs.count
  val modulo : t -> Units.kind -> Defs.power -> Defs.power
  val mul : t -> Units.kind -> Defs.count -> Defs.power
end

val ceil_count : t -> t -> Units.t
val count : t -> t -> Units.t
val heal : Units.kind -> Defs.power -> t -> Defs.power * Defs.power
val of_unit : Units.kind -> Units.t -> t -> Defs.power
val of_units : Units.t -> t -> Defs.power
val min : t -> Defs.power
val max : t -> Defs.power
val pick : t -> Defs.power -> Map.key
val sum : t -> Defs.power
val translate : Units.kind -> Units.kind -> Defs.count -> t -> Defs.count
val untouchable : Units.t -> Units.t -> t -> Units.Set.t

val add : Units.kind -> Defs.power -> t -> t
val attr : Units.Attr.t -> Defs.power -> t -> t
val from_units : Units.t -> t -> t
val map_units : Units.t -> t -> t
val modulo : t -> t -> t
val set : Units.kind -> Defs.power -> t -> t
val set_attr : Units.Attr.t -> Defs.power -> t -> t
val sub : Units.kind -> Defs.power -> t -> t

module Roll : Dice.S -> sig
  val fear : Units.t -> t -> Defs.power
end
