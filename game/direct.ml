module Blessing = struct
  type t = Resource.t
  module Apply (S : State.S) = struct
    let value = S.add_res
  end
  module Make (S : State.S) = struct
    let bless =
      if S.Build.check Build.(ready Temple)
      then Deity.boosted_of
      else Deity.blessing_of
    let value = S.Deity.return bless
  end
end

module BuildManp = struct
  type t = Defs.manpower
  module Apply (S : State.S) = struct
    let value need =
      let avlb = S.Men.get () in
      S.Build.map (Build.manp (min need avlb))
  end
  module Make (S : State.S) = struct
    let value = S.Build.return Build.need_manp
  end
end

module BuildStatus = struct
  type t = Build.status
  module Apply (S : State.S) = struct
    let value status = S.Build.map (Build.update status)
  end
  module Make (S : State.S) = struct
    let value = S.Build.return Build.status
  end
end

module BuildSupply = struct
  type t = Defs.supply
  module Apply (S : State.S) = struct
    let value need =
      let avlb = S.Supply.get () in
      S.Build.map (Build.supp (min need avlb))
  end
  module Make (S : State.S) = struct
    let value = S.Build.return Build.need_supp
  end
end

module Combat = struct
  type t = (module Combat.Outcome)
  module Apply = Combat.Apply
  module Make = Combat.Make
end

module Enemies = struct
  type t = Enemy.t * Enemy.report
  module Apply (S : State.S) = struct
    let value (enemies, _) = S.Enemy.set enemies
  end
  module Make (S : State.S) = struct
    let arriving = S.Turn.return Enemy.spawn
    let enemies = S.Enemy.return (Enemy.combine arriving)
    let scout = S.Scout.return Enemy.to_report
    let report = S.Enemy.return scout
    let value = enemies, report
  end
end

module Starting = struct
  type t = Resource.t
  module Apply (S : State.S) = struct
    let value = S.add_res
  end
  module Make (S : State.S) = struct
    let value = S.Deity.return Deity.starting
  end
end

module Support = struct
  type t = Nation.support list
  module Apply (S : State.S) = struct
    let value ls = S.add_res (Nation.sum ls)
  end
  module Make (S : State.S) = struct
    let bonus = S.Leader.return Leader.res_bonus_of
    let value =
      S.Nation.return Nation.support
      |> Nation.add bonus
  end
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
    let value = S.Supply.sub
  end
  module Make = Upkeep.Make
end

module Victory = struct
  include Event.NoValue
  module Apply (S : State.S) = struct
    let value () = S.Leader.map Leader.won
  end
end
