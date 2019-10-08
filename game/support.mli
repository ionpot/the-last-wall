type chances = Nation.Chance.t
type t = Resource.t Nation.Map.t

val sum : t -> Resource.t

module Apply : State.S -> sig
  val chances : t -> chances -> chances
end

module Roll : State.S -> sig
  val chance_of : Nation.kind -> Nation.t -> float
  val from : Nation.t -> t
end
