open Defs

val strength : float

val too_many : count -> count -> bool
val dr : count -> count -> float

module Check : State.S -> sig
  val value : bool
end

module Make : State.S -> sig
  val value : count
end
