module State = Game.State

module type S = sig
  include Event.S
  module Check : State.S -> sig
    val value : bool
  end
end

module Scout : S with type t = bool = struct
  type t = bool
  module Apply (S : State.S) = struct
    let value x = S.set_scouting x
  end
  module Check (S : State.S) = struct
    let value = S.is_scouting ()
  end
  module Make (S : State.S) = struct
    let value = S.is_scouting ()
  end
end

module Defeat : S with type t = unit = struct
  type t = unit
  module Apply = Event.NoApply
  module Check (S : State.S) = struct
    let value = not (S.has_manp () || S.Cavalry.ptv ())
  end
  module Make = Event.NoValue
end
