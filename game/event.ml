module type Check = State.S -> sig val value : bool end

module type CanCheck = sig
  module Check : Check
end

module type CanMake = sig
  type t
  module Make : State.S -> sig val value : t end
end

module type CanApply = sig
  type t
  module Apply : State.S -> sig val value : t -> unit end
end

module type Direct = sig
  include CanMake
  include CanApply with type t := t
end

type 'a direct = (module Direct with type t = 'a)

module type Conditional = sig
  include Direct
  include CanCheck
end

type 'a cond = (module Conditional with type t = 'a)

module Always (_ : State.S) = struct let value = true end
module Never (_ : State.S) = struct let value = false end

module NoValue = struct
  type t = unit
  module Make (_ : State.S) = struct let value = () end
end

module Empty = struct
  include NoValue
  module Apply (_ : State.S) = struct let value () = () end
  module Check = Never
end

module Apply (State : State.S) = struct
  let value (type a) (x : a) (direct : a direct) =
    let module Event = (val direct) in
    let module Apply = Event.Apply(State) in
    Apply.value x
end

module AddRes (S : State.S) = struct
  let value res =
    S.Supply.add (Resource.sup res);
    S.Units.map Units.(add (Resource.mnp res) Men)
end

module LdrDied (S : State.S) = struct
  let value respawn =
    S.Leader.map (S.Turn.return Leader.died respawn);
    S.Build.map (S.Leader.return Build.died)
end
