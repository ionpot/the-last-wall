type t =
  | Event of Phases.event
  | End

module Make : State.S -> sig
  val first : unit -> t
  val next : t -> t
end
