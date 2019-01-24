type ('e, 'i, 'n) output =
  | Event of 'e
  | Input of 'i
  | Nothing
  | Notify of 'n

type ('c, 'd, 'i, 'n) etype =
  | Cond of 'c
  | Direct of 'd
  | Input of 'i
  | Nothing
  | Notify of 'n

type 'a step =
  | Do of 'a
  | Either of ('a t * 'a t)
  | Last

module type Output = sig
  type event and input and notify
  type t = (event, input, notify) output
end

module type S = sig
  module Output : Output
  type cond and direct and input and notify
  type event = (cond, direct, input, notify) etype
  val steps : event step list
  val event_of : Output.t -> event
  module Apply : State.S -> sig
    val event : Output.event -> unit
    val input : Output.input -> unit
  end
  module Check : sig
    val cond : cond -> (module Event.Check)
    val input : input -> (module Event.Check)
    val notify : notify -> (module Event.Check)
  end
  module Make : State.S -> sig
    val cond : cond -> Output.t
    val direct : direct -> Output.t
    val input : input -> Output.t
    val notify : notify -> Output.t
  end
end
