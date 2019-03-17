type event =
  | Ph1 of Step.Of(Phase1.S).event
  | Ph2 of Step.Of(Phase2.S).event
  | Ph3 of Step.Of(Phase3.S).event

type step =
  | Next of event
  | EndOf of Phase.t
  | End

module Handle : State.S -> sig
  val apply : event -> unit
  val first_of : Phase.t -> step
  val next_of : event -> step
end
