module Of (Phase : Phase.S) : sig
  type apply
  type event =
    | Input of Phase.Input.event
    | Output of (Phase.Output.event * apply)
  type steps
  type t = event * steps

  module Do : State.S -> sig
    val apply : event -> unit
    val first : unit -> t option
    val next : steps -> t option
  end
end
