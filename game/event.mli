module type Direct = sig
  type t
  val apply : t -> State.t -> State.t
  val make : State.t -> t
end

module type Cond = sig
  include Direct
  val check : State.t -> bool
end

type 'a direct = (module Direct with type t = 'a)
type 'a cond = (module Cond with type t = 'a)

val cond : 'a cond -> (State.t -> 'b) -> State.t -> 'b option
