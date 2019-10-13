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
    let value = enemies, scout enemies
  end
end

module Blessing = struct
  type t = Resource.t
  module Apply = Event.AddRes
  module Make (S : State.S) = struct
    module Roll = Deity.Roll(S.Dice)
    let bless =
      if S.Build.check Build.(is_ready Observatory)
      then Roll.boosted
      else Roll.blessing
    let value = S.Deity.return bless
  end
end

module BuildManp = struct
  type t = Defs.manpower
  module Apply (S : State.S) = struct
    let avlb =
      Units.(filter_power Attr.can_build)
      |> S.Units.return
      |> truncate
    let value need =
      S.Build.map (Build.manp need avlb)
  end
  module Make (S : State.S) = struct
    let value = S.Build.return Build.need_manp
  end
end

module BuildStatus = struct
  type t = Build.status
  module Apply (S : State.S) = struct
    let value status =
      S.Build.map (Build.update status)
  end
  module Make (S : State.S) = struct
    let value = S.Build.return Build.status
  end
end

module BuildSupply = struct
  type t = Defs.supply
  module Apply (S : State.S) = struct
    let avlb = S.Supply.get ()
    let value need =
      S.Build.map (Build.supp need avlb);
      S.Supply.set (Number.sub avlb need)
  end
  module Make (S : State.S) = struct
    let value = S.Build.return Build.need_supp
  end
end

module Cavalry = struct
  type t = Defs.count
  let kind = Units.Cavalry
  module Apply (S : State.S) = struct
    module Recruit = Recruit.With(S)
    let value = Recruit.promote kind
  end
  module Make (S : State.S) = struct
    module Recruit = Recruit.With(S)
    let cap = Recruit.Missing.stable ()
    let value = Recruit.affordable kind cap
  end
end

module Combat = Combat

module Facilities = struct
  module Map = Build.Map
  type t = Resource.t Map.t
  let arena = Build.Arena
  module Apply (S : State.S) = struct
    module Add = Event.AddRes(S)
    let value t =
      Map.iter (fun _ -> Add.value) t;
      if Map.mem arena t
      then Map.find arena t |> Resource.manp_of |> S.Arena.set
      else S.Arena.clear ()
  end
  module Make (S : State.S) = struct
    let disease = S.Disease.get ()
    let ldr = S.Leader.get ()
    let merchant = Leader.(is_alive ldr && is Merchant ldr)
    let cha = Leader.cha_mod_of ldr
    let ratio = Float.if_ok (Float.times cha 0.1) merchant
    let bonus = Resource.Bonus.(Add (Sup ratio))
    let to_mnp k = Build.manpwr_range k |> S.Dice.range
    let to_sup k = Build.supply_range k |> S.Dice.range
    let to_res k =
      Resource.bonus_to
      Resource.(empty <+ Supply (to_sup k) <+ Manpwr (to_mnp k))
      Resource.Bonus.(Sub (Both disease))
      |> Resource.bonus_if (k = Build.Market) bonus
    let value =
      S.Build.return Build.ready
      |> Map.mapi (fun k _ -> to_res k)
  end
end

module Starting = Starting

module Starvation = struct
  type deserted = Units.t
  type starved = Units.t
  type t = starved * deserted
  module Apply (S : State.S) = struct
    let value (starved, deserted) =
      S.Units.map Units.(reduce starved);
      S.Units.map Units.(reduce deserted);
      S.Starved.set starved;
      S.Supply.map (max 0)
  end
  module Make (S : State.S) = struct
    module Fill = Units.FillCount(S.Dice)
    let cost = S.Supply.get () |> Number.sub 0
    let starved = S.Units.return (Units.starve cost)
    let portion = S.Dice.rollf 0.75 |> Float.times cost |> truncate
    let deserted = Fill.from portion starved
    let value = Units.reduce deserted starved, deserted
  end
end

module Support = struct
  type t = Support.t
  module Apply (S : State.S) = struct
    module AddRes = Event.AddRes(S)
    module Apply = Support.Apply(S)
    let value t =
      AddRes.value (Support.sum t);
      Apply.chances t
      |> Nation.map_chances
      |> S.Nation.map
  end
  module Make (S : State.S) = struct
    module Roll = Support.Roll(S)
    let value = S.Nation.return Roll.from
  end
end

module Turn = struct
  type t = Defs.turn * Month.t * Weather.t
  module Apply (S : State.S) = struct
    let value (t, m, w) =
      S.Casualty.clear ();
      S.Turn.set t;
      S.Month.set m;
      S.Weather.set w
  end
  module Make (S : State.S) = struct
    module Weather = Weather.Roll(S.Dice)
    let month = S.Month.return Month.next
    let value = S.Turn.next (), month, Weather.random month
  end
end

module Upkeep = struct
  type t = Defs.supply
  module Apply (S : State.S) = struct
    let value = S.Supply.sub
  end
  module Make (S : State.S) = struct
    let cost = S.Units.return Units.upkeep
    let scouts = S.Scout.return (Number.if_ok 10)
    let ldr = S.Leader.get ()
    let cha = Leader.cha_mod_of ldr
    let bonus = Float.times cha 0.03
    let ratio =
      Leader.(is_alive ldr && is Engineer ldr)
      |> Float.if_ok bonus
    let value = Number.reduce_by ratio (cost + scouts)
  end
end

module Victory = struct
  include Event.NoValue
  module Apply (S : State.S) = struct
    let value _ = S.Leader.map Leader.won
  end
end
