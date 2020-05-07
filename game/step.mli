type kind =
  | Input of Input.kind
  | Output of Output.kind
type t

val first : State.t -> t option
val next : t -> t option

val kind : t -> kind
val state : t -> State.t
val state_set : State.t -> t -> t
