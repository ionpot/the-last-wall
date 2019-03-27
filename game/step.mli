module Of (Phase : Phase.S) : sig
  type apply
  type output =
    | Event of (Phase.Output.event * apply)
    | Input of Phase.Output.input
    | Notify of Phase.Output.notify
  type steps
  type event = output * steps

  module Do : State.S -> sig
    val apply : output -> unit
    val first : unit -> event option
    val next : steps -> event option
  end
end
