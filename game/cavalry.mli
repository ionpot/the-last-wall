open Defs

val strength : manpower

val too_many : manpower -> manpower -> bool

module Check (M: State.S) : sig
  val value : manpower option
end

module Dr (M: State.S) : sig
  val value : float
end

module Casualty (M: State.S) : sig
  val check : manpower -> manpower
end
