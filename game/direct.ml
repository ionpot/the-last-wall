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
    let value t = S.Build.map (Build.apply_mnp t)
  end
  module Make (S : State.S) = struct
    let units = S.Units.return Units.(filter Attr.can_build)
    let base = S.Bonus.return Power.base
    let wrp = Power.of_units units base |> truncate
    let res = S.Build.return Build.needs
    let value = min wrp (Resource.manp_of res)
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
    let value t =
      let t', b = S.Build.return (Build.apply_sup t) in
      S.Build.set b;
      S.Supply.set t'
  end
  module Make (S : State.S) = struct
    let sup = S.Supply.get ()
    let res = S.Build.return Build.needs
    let value = min sup (Resource.supp_of res)
  end
end

module Cavalry = Recruit.Event(struct
  let kind = Units.Cavalry
  let action = Recruit.New
  let pool = None
  module Cap = Recruit.NoCap
end)

module Combat = Combat

module Facilities = struct
  module Map = Build.Map
  type t = Resource.t Map.t
  let arena = Build.Arena
  module Apply (S : State.S) = struct
    module Add = Event.AddRes(S)
    let arena_mnp t =
      if Map.mem arena t
      then Map.find arena t |> Resource.manp_of
      else 0
    let value t =
      Map.iter (fun _ -> Add.value) t;
      let n = arena_mnp t in
      S.Pool.map Pool.(add Arena n)
  end
  module Make (S : State.S) = struct
    let disease = S.Mishap.check Mishap.(has Disease)
      |> Float.if_ok 0.2
    let merchant = S.Leader.check Leader.(is_living Merchant)
    let cha = S.Leader.return Leader.cha_mod_of
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

module Fear = struct
  type t = Units.t
  module Apply (S : State.S) = struct
    let value fled =
      S.Feared.set fled;
      S.Units.map (Units.reduce fled)
  end
  module Make (S : State.S) = struct
    module Fill = Dist.Fill(S.Dice)
    module Roll = Power.Roll(S.Dice)
    let e = S.Enemy.return Units.(filter Attr.can_fear)
    let base = S.Bonus.return Power.base
    let cap = Roll.fear e base
    let units = S.Units.return Units.(discard Attr.is_siege)
    let value = Fill.from cap base units |> fst
  end
end

module FearEnd = struct
  type t = Units.t
  module Apply (S : State.S) = struct
    let value t =
      S.Units.map (Units.combine t);
      S.Feared.clear ()
  end
  module Make (S : State.S) = struct
    let value = S.Feared.get ()
  end
end

module Mishap = struct
  type t = Mishap.t
  module Apply (S : State.S) = struct
    let value t =
      S.Mishap.set t;
      if Mishap.(has Tavern) t
      then S.Build.map Build.(raze Tavern)
  end
  module Make (S : State.S) = struct
    module Roll = Mishap.Roll(S.Dice)
    let units = S.Units.return Units.(filter_count Attr.is_infectable)
    let check = function
      | Mishap.Comet -> S.Turn.has 5
      | Mishap.Disease -> units >= 50
      | Mishap.Tavern -> S.Build.check Build.(is_ready Tavern)
    let value = Roll.from check
  end
end

module NationSupport = struct
  type t = Support.t
  module Apply (S : State.S) = struct
    module AddRes = Event.AddRes(S)
    module Apply = Support.Apply(S)
    let value t =
      AddRes.value (Support.sum t);
      Apply.value t
  end
  module Make (S : State.S) = struct
    module Roll = Support.Roll(S)
    let value = S.Nation.return Roll.from
  end
end

module ResearchProgress = struct
  module type Progress = Research.Progress
  type t = (module Progress)
  module Apply (S : State.S) = struct
    let handle (module P : Progress) =
      S.Supply.set P.rem_supply;
      S.Units.map Units.(sub P.men_used Men)
    let value p =
      handle p;
      S.Research.get ()
      |> Research.set_progress p
      |> Research.tick
      |> S.Research.set
  end
  module Make (S : State.S) = struct
    let men = S.Units.return Units.(count Men)
    let supply = S.Supply.get ()
    let value =
      Research.apply men supply
      |> S.Research.return
  end
end

module ResearchStatus = struct
  type t = Research.Set.t
  module Apply (S : State.S) = struct
    let value s =
      Research.set_complete s
      |> S.Research.map
  end
  module Make (S : State.S) = struct
    let value = S.Research.return Research.complete
  end
end

module Revive = struct
  type t = Units.t * Units.t
  module Apply (S : State.S) = struct
    let value (revived, rem) =
      S.Units.map (Units.combine revived);
      S.Casualty.set rem
  end
  module Make (S : State.S) = struct
    module Fill = Dist.Fill(S.Dice)
    let units = S.Units.get ()
    let base = S.Bonus.return Power.base
    let pwr = Power.revive units |> Power.of_units units
    let value =
      Units.(filter Attr.is_revivable)
      |> S.Casualty.return
      |> Fill.from pwr base
  end
end

module Starting = Starting

module Starvation = struct
  type deserted = Units.t
  type starved = Units.t
  type t = deserted * starved
  module Apply (S : State.S) = struct
    let value (deserted, starved) =
      S.Units.map Units.(reduce starved);
      S.Units.map Units.(reduce deserted);
      S.Starved.set starved;
      S.Supply.map (max 0);
      Units.count_all starved
      |> Nation.Chance.sub_all
      |> Nation.map_chances
      |> S.Nation.map
  end
  module Make (S : State.S) = struct
    module Fill = Units.Fill(S.Dice)
    let cost = S.Supply.get () |> Number.sub 0
    let starved = S.Units.return (Units.starve cost)
    let portion = S.Dice.rollf 0.75 |> Float.times cost |> truncate
    let value = Fill.from portion starved
  end
end

module Turn = struct
  type t = Defs.turn * Month.t * Weather.t
  module Apply (S : State.S) = struct
    let set_bonus w =
      let clear = w = Weather.Clear in
      S.Bonus.map Bonus.(set ClearSky clear)
    let value (t, m, w) =
      S.Casualty.clear ();
      S.Turn.set t;
      S.Month.set m;
      S.Weather.set w;
      set_bonus w
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
    module Check = Support.Check(S)
    let units =
      let army = S.Research.check Research.(is_complete BlackArmy) in
      let mercs = Float.if_ok 0.1 army in
      let tulron = Check.has_traded Nation.Tulron in
      let cavs = Float.if_ok 0.2 tulron in
      let bonus = Units.Bonus.(empty
        |> attr Units.Attr.is_cavalry cavs
        |> kind Units.Merc mercs)
      in S.Units.return (Units.upkeep bonus)
    let scouting = S.Scout.return (Number.if_ok 10)
    let engineer = S.Leader.check Leader.(is_living Engineer)
    let cha = S.Leader.return Leader.cha_mod_of
    let bonus = Float.times cha 0.03
    let ratio = Float.if_ok bonus engineer
    let value = Number.reduce_by ratio (units + scouting)
  end
end

module Victory = struct
  include Event.NoValue
  module Apply (S : State.S) = struct
    let value _ = S.Leader.map Leader.won
  end
end
