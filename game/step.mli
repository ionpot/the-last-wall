module Of (Phase : Phase.S) : sig
  type output =
    | Event of Phase.Output.event
    | Input of Phase.Output.input
    | Notify of Phase.Output.notify
  type steps
  type event = output * steps

  module Apply : State.S -> sig
    val value : output -> unit
  end

  module Seek : State.S -> sig
    val first : unit -> event option
    val next : steps -> event option
  end
end
