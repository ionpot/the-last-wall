module Attack = struct
  type t = Units.t * Attack.report
  module Apply (S : State.S) = struct
    let value (enemies, _) =
      S.Enemy.set enemies;
      if Units.(has Harpy enemies)
      then S.Harpy.clear ()
      else S.Harpy.add 0.1
  end
  module Make (S : State.S) = struct
    module Attack = Attack.Make(S)
    let arriving = S.Turn.return Attack.roll
    let enemies = S.Enemy.return (Units.combine arriving)
    let scout = S.Scout.return Attack.report
    let report = S.Enemy.return scout
    let value = enemies, report
  end
end

module Blessing = struct
  type t = Resource.t
  module Apply = Event.AddRes
  module Make (S : State.S) = struct
    module Roll = Deity.Roll(S.Dice)
    let bless =
      if S.Build.check Build.(ready Observatory)
      then Roll.boosted
      else Roll.blessing
    let value = S.Deity.return bless
  end
end

module BuildManp = struct
  type t = Defs.manpower
  module Apply (S : State.S) = struct
    let value need =
      let avlb = S.Units.return Units.(count Men) in
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

module CanBarrage = struct
  type reason = Leader | Weather
  type t = Yes | No of reason
  module Apply (S : State.S) = struct
    let value t = S.Barraging.set_to (t = Yes)
  end
  module Make (S : State.S) = struct
    let value =
      if S.Leader.check Leader.is_dead then No Leader
      else if S.Weather.check Weather.is_bad then No Weather
      else Yes
  end
end

module Combat = struct
  type t = (module Combat.Outcome)
  module Apply = Combat.Apply
  module Make = Combat.Make
end

module Starting = struct
  type t = Month.t * Resource.t
  module Apply (S : State.S) = struct
    module AddRes = Event.AddRes(S)
    let value (m, r) =
      S.Month.set m;
      AddRes.value r
  end
  module Make (S : State.S) = struct
    module Deity = Deity.Roll(S.Dice)
    module Month = Month.Roll(S.Dice)
    let value =
      Month.random (),
      S.Deity.return Deity.starting
  end
end

module Support = struct
  type t = Nation.support
  module Apply (S : State.S) = struct
    module AddRes = Event.AddRes(S)
    let value ls = AddRes.value (Nation.sum ls)
  end
  module Make (S : State.S) = struct
    module Roll = Nation.Roll(S.Dice)
    let bonus = S.Leader.return Leader.res_bonus_of
    let value =
      S.Nation.return Roll.support
      |> Nation.add bonus
  end
end

module Turn = struct
  type t = Defs.turn * Month.t * Weather.t
  module Apply (S : State.S) = struct
    let value (t, m, w) =
      S.Turn.set t;
      S.Month.set m;
      S.Weather.set w
  end
  module Make (S : State.S) = struct
    module Weather = Weather.Roll(S.Dice)
    let value =
      S.Turn.next (),
      S.Month.return Month.next,
      S.Month.return Weather.random
  end
end

module Upkeep = struct
  type t = Defs.supply
  module Apply (S : State.S) = struct
    let value = S.Supply.sub
  end
  module Make (S : State.S) = struct
    let cost = S.Units.return Units.upkeep
    let scouts = S.Scout.either 10 0
    let value = cost + scouts
  end
end

module Victory = struct
  include Event.NoValue
  module Apply (S : State.S) = struct
    let value _ = S.Leader.map Leader.won
  end
end
