open Defs

val strength : manpower

val too_many : manpower -> manpower -> bool

module Check : State.S -> sig
  val value : manpower option
end

module Make : State.S -> sig
  val value : count
end

module Dr (M: State.S) : sig
  val value : float
end
