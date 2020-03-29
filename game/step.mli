type kind =
  | Input of Input.event
  | Output of (Output.event * State.t)
type ls
type t = kind * ls

val start : ls

val next : State.t -> ls -> t option
