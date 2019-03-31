module Build = struct
  type t = Building.t list
  module Apply (S : State.S) = struct
    let value = S.build
  end
  module Make (S : State.S) = struct
    let value = []
  end
end

module Deity = struct
  type t = Deity.t
  module Apply (S : State.S) = struct
    let value = S.Deity.set
  end
  module Make (S : State.S) = struct
    let value = Deity.empty
  end
end

module Leader = struct
  type t = Leader.ltype
  module Apply (S : State.S) = struct
    let value ltype = S.Leader.set (Leader.make ltype)
  end
  module Make (S : State.S) = struct
    let value = Leader.Aristocrat
  end
end

module Mercs = struct
  type t = Defs.manpower
  module Apply (S : State.S) = struct
    let value = S.supp2manp
  end
  module Check (S : State.S) = struct
    let value = Dice.chance 0.8
  end
  module Make (S : State.S) = struct
    let value = Dice.between 10 30
  end
end

module Nations = struct
  type t = Nation.t list
  module Apply (S : State.S) = struct
    let value = S.set_nats
  end
  module Make (S : State.S) = struct
    let value = S.get_nats ()
  end
end

module Scout = struct
  type t = bool
  module Apply (S : State.S) = struct
    let value = S.set_scouting
  end
  module Make (S : State.S) = struct
    let value = S.is_scouting ()
  end
end
