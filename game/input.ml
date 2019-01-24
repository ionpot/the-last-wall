module type S = Event.Conditional

module Build : S with type t = Building.t list = struct
  type t = Building.t list
  module Apply (S : State.S) = struct
    let value x = S.build x
  end
  module Check = Event.Always
  module Make (S : State.S) = struct
    let value = []
  end
end

module Nations : S with type t = Nation.t list = struct
  type t = Nation.t list
  module Apply (S : State.S) = struct
    let value x = S.set_nats x
  end
  module Check = Event.Always
  module Make (S : State.S) = struct
    let value = S.get_nats ()
  end
end

module Scout : S with type t = bool = struct
  type t = bool
  module Apply (S : State.S) = struct
    let value x = S.set_scouting x
  end
  module Check = Event.Always
  module Make (S : State.S) = struct
    let value = S.is_scouting ()
  end
end
