type event =
  | Ph1 of Step.Of(Phase1).t
  | Ph2 of Step.Of(Phase1).t
  | Ph3 of Step.Of(Phase1).t

type step =
  | Next of event
  | EndOf of Phase.t

module Handle : State.S -> sig
  val apply : event -> unit
  val first_of : Phase.t -> step
  val next_of : event -> step
end
