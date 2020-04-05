type kind =
  | Input of Input.kind
  | Output of Output.kind
type t

val next : State.t -> Steps.t list -> t option

val kind : t -> kind
val rest : t -> Steps.t list
val state : t -> State.t
