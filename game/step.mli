type kind =
  | Input of Input.event
  | Output of (Output.event * State.t)
type steps
type t = kind * steps

val start : steps

val next : State.t -> steps -> t option
