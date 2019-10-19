type chances = Nation.Chance.t
type t = Nation.support

val sum : t -> Resource.t

module Apply : State.S -> sig
  val value : t -> unit
end

module Check : State.S -> sig
  val has_traded : Nation.kind -> bool
  val traded_mnp : Nation.kind -> Defs.manpower
  val traded_sup : Nation.kind -> Defs.supply
end

module Roll : State.S -> sig
  val chance_of : Nation.kind -> Nation.t -> Defs.chance
  val from : Nation.t -> t
end
