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

module Input : sig
  type 'a t
  val make : 'a direct -> State.t -> 'a t
  val apply : 'a t -> (State.t -> 'a -> State.t)
  val value : 'a t -> 'a
end

module Output : sig
  val make : 'a direct -> State.t -> 'a * State.t
end
