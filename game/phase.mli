type ('e, 'i, 'n) output =
  | Event of 'e
  | Input of 'i
  | Notify of 'n

type ('c, 'd, 'i, 'n) etype =
  | Cond of 'c
  | Direct of 'd
  | Input of 'i
  | Notify of 'n

type 'a step =
  | Do of 'a
  | Either of ('a * 'a)
  | JumpIfNo of ('a * 'a)

module type Output = sig
  type event and input and notify
  type t = (event, input, notify) output
end

module type Steps = sig
  module Output : Output
  type cond and direct and input and notify
  type event = (cond, direct, input, notify) etype
  type t = event step list
  val steps : t
  val is_end : Output.t -> bool
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
    val cond : cond -> Output.event
    val direct : direct -> Output.event
    val input : input -> Output.input
    val notify : notify -> Output.notify
  end
end
