module Barrage = struct
  type t = bool
  module Apply (S : State.S) = struct
    let value = S.Barraging.set_to
  end
  module Check (S : State.S) = struct
    let value = S.Barraging.get ()
  end
  module Make (S : State.S) = struct
    let value = S.Barraging.get ()
  end
end

module Build = struct
  type t = Build.kind list
  module Apply (S : State.S) = struct
    let value ls = S.Build.map (Build.start ls)
  end
  module Make (S : State.S) = struct
    let value = S.Build.return Build.available
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
  type t = Leader.kind
  module Apply (S : State.S) = struct
    let value kind = S.Leader.set (Leader.make kind)
  end
  module Make (S : State.S) = struct
    let value = Leader.Aristocrat
  end
end

module Mercs = struct
  type t = Defs.manpower
  module Apply (S : State.S) = struct
    let value men =
      S.Supply.sub men;
      S.Men.add men
  end
  module Check (S : State.S) = struct
    let value = Dice.chance 0.8
  end
  module Make (S : State.S) = struct
    let value = Dice.between 10 30
  end
end

module Nations = struct
  type t = Nation.kind list
  module Apply (S : State.S) = struct
    let value ls = S.Nation.set (Nation.from ls)
  end
  module Make (S : State.S) = struct
    let value = S.Nation.return Nation.which
  end
end

module Scout = struct
  type t = bool
  module Apply (S : State.S) = struct
    let value = S.Scout.set_to
  end
  module Make (S : State.S) = struct
    let value = S.Scout.get ()
  end
end
