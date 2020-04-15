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

let cond : type a. a cond -> (State.t -> 'b) -> State.t -> 'b option =
  fun (module M) f state ->
    if M.check state then Some (f state) else None
