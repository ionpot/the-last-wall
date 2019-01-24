module State = Game.State
module Defs = Game.Defs

module type S = sig
  type t
  module Apply (S : State.S) : sig
    val value : t -> unit
  end
  module Make (S : State.S) : sig
    val value : t
  end
end

module NoApply (S : State.S) = struct
  let value () = ()
end

module NoValue (S : State.S) = struct
  let value = ()
end

module Turn : S with type t = Defs.turn = struct
  type t = Defs.turn
  module Apply (S : State.S) = struct
    let value x = S.set_turn x
  end
  module Make (S : State.S) = struct
    let value = S.get_turn () + 1
  end
end
