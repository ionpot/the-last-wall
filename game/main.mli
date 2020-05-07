type step =
  | Next of Step.t
  | End

val first : State.t -> step
val next : step -> step
