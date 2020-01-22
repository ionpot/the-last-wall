type chances = Nation.Chance.t
type t = Nation.support

val sum : t -> Resource.t

module Apply : State.S -> sig
  val value : t -> unit
end

module Roll : State.S -> sig
  val chance_of : Nation.kind -> Nation.t -> Defs.percent
  val from : Nation.t -> t
end
