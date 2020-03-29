type step =
  | Next of Step.t
  | End

val first : State.t -> step
val next : Step.ls -> State.t -> step
