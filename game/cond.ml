module Cavalry = struct
  type t = Defs.count
  module Apply (S : State.S) = struct
    let value =
      Listx.apply_to [S.Cavalry.add; S.sub_manp; S.sub_supp]
  end
  module Check = Cavalry.Check
  module Make = Cavalry.Make
end

module Defeat = struct
  include Event.NoValue
  module Apply (S : State.S) = struct
    let value = S.Ended.set
  end
  module Check (S : State.S) = struct
    let value =
      S.no_manp () && S.Cavalry.is_zero ()
  end
end

module LeaderDied = struct
  include Event.NoValue
  module Apply (S : State.S) = struct
    let died = S.Turn.return Leader.died
    let value () = S.Leader.map died
  end
  module Check (S : State.S) = struct
    let value =
      S.Leader.check Leader.is_alive
      && Dice.chance 0.05
  end
end

module LeaderLvUp = struct
  include Event.NoValue
  module Apply (S : State.S) = struct
    let value () = S.Leader.map Leader.lvup
  end
  module Check (S : State.S) = struct
    let value = S.Leader.check Leader.can_lvup
  end
end

module LeaderNew = struct
  type t = Leader.t
  module Apply (S : State.S) = struct
    let value = S.Leader.set
  end
  module Check (S : State.S) = struct
    let can_respawn = S.Turn.return Leader.can_respawn
    let value = S.Leader.check can_respawn
  end
  module Make (S : State.S) = struct
    let value = Leader.random ()
  end
end

module Market = struct
  type t = Defs.supply
  module Apply (S : State.S) = struct
    let value = S.add_supp
  end
  module Check (S : State.S) = struct
    let value = S.bld_ready Building.Market
  end
  module Make (S : State.S) = struct
    let value = Dice.between 15 45
  end
end

module Starvation = struct
  type t = Defs.manpower * Defs.manpower
  module Apply (S : State.S) = struct
    let value (men, cav) =
      S.sub_manp men;
      S.Cavalry.sub cav;
      S.clr_supp ()
  end
  module Check (S : State.S) = struct
    let value = S.get_supp () < 0
  end
  module Make = Upkeep.Starvation
end
