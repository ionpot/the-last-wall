type event =
  | Ph1 of (Phase1.Output.t * Phase1.Steps.t)
  | Ph2 of (Phase2.Output.t * Phase2.Steps.t)
  | Ph3 of (Phase3.Output.t * Phase3.Steps.t)
  | End

module First : State.S -> sig
  val value : event
end

module Next : State.S -> sig
  val value : event -> event
end
