module Map = Units.Map

type t = Defs.power Map.t

val empty : t

val base : Bonus.t -> t
val dr : Units.t -> t

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
val sum : t -> Defs.power
val translate : Units.kind -> Units.kind -> Defs.count -> t -> Defs.count
val untouchable : Units.t -> Units.t -> t -> Units.Set.t

val add : Units.kind -> Defs.power -> t -> t
val from_units : Units.t -> t -> t
val map_units : Units.t -> t -> t
val modulo : t -> t -> t
val sub : Units.kind -> Defs.power -> t -> t