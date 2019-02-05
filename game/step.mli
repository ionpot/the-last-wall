module Next (Steps : Phase.Steps) : State.S -> sig
  val value : Steps.t -> (Steps.Output.t * Steps.t) option
end
