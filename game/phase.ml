type t = One | Two | Three

module Apply (State : State.S) = struct
  let value (type a) (x : a) (module Event : Event.CanApply with type t = a) =
    let module A = Event.Apply(State) in A.value x
end

module type Direct = sig
  module Event : Event.Direct
  type t
  val make : Event.t -> t
end

module type Cond = sig
  module Event : Event.Conditional
  type t
  val make : Event.t -> t
end

module type Notify = sig
  module Event : Event.Notify
  type t
  val make : Event.t -> t
end

module type S = sig
  module Steps : Steps.S

  module Input : sig
    type event
    module type Cond = Cond with type t := event
    module type Direct = Direct with type t := event
    val direct : Steps.Input.direct -> (module Direct)
    val cond : Steps.Input.cond -> (module Cond)
    module Apply : State.S -> sig
      val event : event -> unit
    end
  end

  module Output : sig
    type event
    module type Cond = Cond with type t := event
    module type Direct = Direct with type t := event
    module type Notify = Notify with type t := event
    val direct : Steps.Output.direct -> (module Direct)
    val cond : Steps.Output.cond -> (module Cond)
    val notify : Steps.Output.notify -> (module Notify)
  end
end
