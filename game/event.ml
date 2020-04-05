module type Direct = sig
  type t
  val apply : State.t -> t -> State.t
  val make : State.t -> t
end

module type Cond = sig
  include Direct
  val check : State.t -> bool
end

type 'a direct = (module Direct with type t = 'a)
type 'a cond = (module Cond with type t = 'a)

module Input = struct
  type 'a t =
    { apply : State.t -> 'a -> State.t
    ; value : 'a
    }
  let make (type a) (module M : Direct with type t = a) state =
    { apply = M.apply
    ; value = M.make state
    }
  let apply t = t.apply
  let value t = t.value
end

module Output = struct
  let make (type a) (module M : Direct with type t = a) state =
    let x = M.make state in
    x, M.apply state x
end
