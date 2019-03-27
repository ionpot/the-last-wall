type t = One | Two | Three

module type Output = sig
  type event and input and notify
  module Apply : State.S -> sig
    val input : input -> unit
  end
end

module Direct (Output : Output) = struct
  type 'a convert = 'a Event.direct -> Output.event
  type 'a t = 'a Event.direct * 'a convert
  let make (type a) (m: a Event.direct) (fn : a convert) : a t = m, fn
end

module type S = sig
  module Output : Output
  module Steps : Steps.S
  module Make : State.S -> sig
    val cond : Steps.cond -> Output.event
    val direct : Steps.direct -> 'a Direct(Output).t
    val input : Steps.input -> Output.input
    val notify : Steps.notify -> Output.notify
  end
end
