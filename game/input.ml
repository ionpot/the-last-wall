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

module BuildAvlb = struct
  type t = Build.kind list
  module Apply (S : State.S) = struct
    module Bonus = Build_bonus.From(S)
    let value ls = S.Build.map (Build.start ls Bonus.value)
  end
  module Make (S : State.S) = struct
    let value = S.Build.return Build.ls_avlb
  end
end

module DeityChoice = struct
  type t = Deity.t
  module Apply (S : State.S) = struct
    let value = S.Deity.set
  end
  module Make (S : State.S) = struct
    let value = Deity.empty
  end
end

module Dervish = struct
  type t = Defs.count
  let each = 6
  let kind = Units.Dervish
  module Apply (S : State.S) = struct
    module Temple = Temple.With(S)
    let value n =
      Temple.buy kind n;
      S.Dervish.set n
  end
  module Make (S : State.S) = struct
    module Temple = Temple.With(S)
    let cap = Temple.cap_for kind
    let value = S.Dice.roll (min each cap)
  end
end

module Leader = struct
  type t = Leader.kind
  module Apply (S : State.S) = struct
    module Roll = Leader.Roll(S.Dice)
    let value kind = S.Leader.set (Roll.from kind)
  end
  module Make (S : State.S) = struct
    let value = Leader.(kind_of empty)
  end
end

module Mercs = struct
  type t = Defs.count
  module Apply (S : State.S) = struct
    let value count =
      let count' = S.Supply.take count in
      S.Units.map Units.(add count' Men)
  end
  module Check (S : State.S) = struct
    let value = S.Dice.chance 0.8
  end
  module Make (S : State.S) = struct
    let value = S.Dice.between 10 30
  end
end

module Nations = struct
  type t = Nation.kind list
  module Apply (S : State.S) = struct
    let value ls = S.Nation.map (Nation.chosen ls)
  end
  module Make (S : State.S) = struct
    let value = S.Nation.return Nation.which
  end
end

module Ranger = struct
  type t = Defs.count
  let kind = Units.Ranger
  module Apply (S : State.S) = struct
    module Temple = Temple.With(S)
    let value = Temple.promote kind
  end
  module Check (S : State.S) = struct
    let value = S.Deity.is Deity.Sitera
  end
  module Make (S : State.S) = struct
    module Temple = Temple.With(S)
    let value = Temple.promotable kind
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

module Templar = struct
  type t = Defs.count
  let kind = Units.Templar
  module Apply (S : State.S) = struct
    module Temple = Temple.With(S)
    let value = Temple.promote kind
  end
  module Check (S : State.S) = struct
    let value = not (S.Deity.is Deity.Sitera)
  end
  module Make (S : State.S) = struct
    module Temple = Temple.With(S)
    let value = Temple.promotable kind
  end
end

module Trade = struct
  type t = Nation.trade
  let none = Nation.NoTrade
  module Apply (S : State.S) = struct
    let value trade =
      S.Build.map (Build.set_trade trade)
  end
  module Check (S : State.S) = struct
    let value = S.Build.check Build.(built (Trade none))
  end
  module Make (S : State.S) = struct
    let value = none
  end
end
