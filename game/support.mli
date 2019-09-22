type chances = Nation.Chance.t
type t

val ls : t -> (Nation.kind * Resource.t) list
val sum : t -> Resource.t
val update_chances : t -> chances -> chances

module Roll : State.S -> sig
  val chance_of : Nation.kind -> Nation.t -> float
  val from : Nation.t -> t
end