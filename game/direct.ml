module Blessing = struct
  type t = Resource.t
  module Apply (S : State.S) = struct
    let value = S.add_res
  end
  module Make (S : State.S) = struct
    let bless =
      if S.bld_ready Building.Temple
      then Deity.boosted_of
      else Deity.blessing_of
    let value = S.with_deity bless
  end
end

module BuildManp = struct
  type t = Defs.manpower
  module Apply (S : State.S) = struct
    let value = S.bld_manp
  end
  module Make (S : State.S) = struct
    let value = S.bld_manp_cost ()
  end
end

module BuildStatus = struct
  type t = Buildings.status
  module Apply (S : State.S) = struct
    let value = S.bld_update
  end
  module Make (S : State.S) = struct
    let value = S.bld_status ()
  end
end

module BuildSupply = struct
  type t = Defs.supply
  module Apply (S : State.S) = struct
    let value = S.bld_supp
  end
  module Make (S : State.S) = struct
    let value = S.bld_supp_cost ()
  end
end

module Enemies = struct
  type t = Enemy.party list * Enemy.report
  module Apply (S : State.S) = struct
    let value (enemies, _) = S.set_enemies enemies
  end
  module Make (S : State.S) = struct
    let enemies = S.Turn.return Enemy.spawn
    let report = Enemy.to_report enemies (S.is_scouting ())
    let value = enemies, report
  end
end

module Starting = struct
  type t = Resource.t
  module Apply (S : State.S) = struct
    let value = S.add_res
  end
  module Make (S : State.S) = struct
    let value = Deity.starting (S.get_deity ())
  end
end

module Support = struct
  type t = Nation.support list
  module Apply (S : State.S) = struct
    let value nats = S.add_res (Nation.total_of nats)
  end
  module Make = Check_support.Make
end

module Turn = struct
  type t = Defs.turn
  module Apply (S : State.S) = struct
    let value = S.Turn.set
  end
  module Make (S : State.S) = struct
    let value = S.Turn.next ()
  end
end

module Upkeep = struct
  type t = Defs.supply
  module Apply (S : State.S) = struct
    let value = S.sub_supp
  end
  module Make = Upkeep.Make
end
