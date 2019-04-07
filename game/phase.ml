type t = One | Two | Three

module type Definitions = sig type event end

module Convert = struct
  module type Check = sig
    module Check : Event.Check
    type t val value : t
  end
  module type Cond = sig
    module Event : Event.Conditional
    type t val make : Event.t -> t
  end
  module type Direct = sig
    module Event : Event.Direct
    type t val make : Event.t -> t
  end
  module Input (Steps : Steps.Input) (Input : Definitions) = struct
    module type Cond = Cond with type t := Input.event
    module type Direct = Direct with type t := Input.event
    type cond = Steps.cond -> (module Cond)
    type direct = Steps.direct -> (module Direct)
  end
  module Output (Steps : Steps.Output) (Output : Definitions) = struct
    module type Check = Check with type t := Output.event
    module type Cond = Cond with type t := Output.event
    module type Direct = Direct with type t := Output.event
    type check = Steps.check -> (module Check)
    type cond = Steps.cond -> (module Cond)
    type direct = Steps.direct -> (module Direct)
  end
end

module Apply (State : State.S) = struct
  let value (type a) (x : a) (module Event : Event.CanApply with type t = a) =
    let module A = Event.Apply(State) in A.value x
end

module type S = sig
  module Steps : Steps.S
  module Input : sig
    include Definitions
    module Apply : State.S -> sig
      val event : event -> unit
    end
  end
  module Output : Definitions
  module Convert : sig
    module Input : sig
      val cond : Convert.Input(Steps.Input)(Input).cond
      val direct : Convert.Input(Steps.Input)(Input).direct
    end
    module Output : sig
      val check : Convert.Output(Steps.Output)(Output).check
      val cond : Convert.Output(Steps.Output)(Output).cond
      val direct : Convert.Output(Steps.Output)(Output).direct
    end
  end
end
