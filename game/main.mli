type event =
  | Input of Input.event
  | Mark of Steps.Label.t
  | Output of Output.event

type t

val first : State.t -> t option
val next : t -> t option

val event : t -> event
val state : t -> State.t
val state_set : State.t -> t -> t
