type t = One | Two | Three

module type Output = sig
  type event and input and notify
  module Apply : State.S -> sig
    val event : event -> unit
    val input : input -> unit
  end
end

module type S = sig
  module Output : Output
  module Steps : Steps.S
  module Check : sig
    val cond : Steps.cond -> (module Event.Check)
    val input : Steps.input -> (module Event.Check)
    val notify : Steps.notify -> (module Event.Check)
  end
  module Make : State.S -> sig
    val cond : Steps.cond -> Output.event
    val direct : Steps.direct -> Output.event
    val input : Steps.input -> Output.input
    val notify : Steps.notify -> Output.notify
  end
end
