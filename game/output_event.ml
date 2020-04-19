module Starting = Starting

(*
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
    let units = S.Units.return Units.(filter Attr.build)
    let wrp = Units.power_of units |> truncate
    let res = S.Build.return Build.needs
    let value = min wrp (Resource.mnp res)
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
    let value _ =
      let sup = S.Supply.get () in
      let sup', b = S.Build.return (Build.apply_sup sup) in
      S.Build.set b;
      S.Supply.set sup'
  end
  module Make (S : State.S) = struct
    let value = S.Build.return Build.needs |> Resource.sup
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
      then Map.find arena t |> Resource.mnp
      else 0
    let value t =
      Map.iter (fun _ -> Add.value) t;
      let n = arena_mnp t in
      S.Pool.map Pool.(add Arena n)
  end
  module Make (S : State.S) = struct
    module Bonus = Bonus.Make(S)
    let to_mnp k = Build.manpwr_range k |> S.Dice.range
    let to_sup k = Build.supply_range k |> S.Dice.range
    let to_res k =
      Resource.make ~mnp:(to_mnp k) ~sup:(to_sup k) ()
      |> Bonus.resource_disease
      |> Bonus.market_boost k
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
    let e = S.Enemy.return Units.(filter Attr.fear)
    let base = Power.base
    let cap = Roll.fear e base
    let units = S.Units.return Units.(discard Attr.siege)
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
    let units = S.Units.return Units.(filter_count Attr.infectable)
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
    let base = Power.base
    let pwr = Power.revive units |> Power.of_units units
    let value =
      Units.(filter Attr.revivable)
      |> S.Casualty.return
      |> Fill.from pwr base
  end
end

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
    module Bonus = Bonus.Make(S)
    let units = S.Units.return Units.upkeep
      |> Bonus.upkeep_units
    let scouting = S.Scout.return (Number.if_ok 10)
      |> Bonus.upkeep_scouting
    let value = (units + scouting)
      |> Bonus.upkeep_engr
  end
end

module Victory = struct
  include Event.NoValue
  module Apply (S : State.S) = struct
    let value _ = S.Leader.map Leader.won
  end
end

=== COND ===

module Ballista = struct
  type t = Units.t * Units.t
  module Apply (S : State.S) = struct
    let value (_, remaining) = S.Enemy.set remaining
  end
  module Check = Check.NoFog
  module Make (S : State.S) = struct
    module Bonus = Bonus.Make(S)
    module Fill = Dist.Fill(S.Dice)
    let ballista = S.Units.return (Units.only Ballista)
    let power = Bonus.artillery (Power.artillery ballista)
    let damage = Power.of_units ballista power
    let value = S.Enemy.return (Fill.from damage Power.base)
  end
end

module Barraged = struct
  type t = Units.t
  module Apply (S : State.S) = struct
    let value u = S.Enemy.map (Units.reduce u)
  end
  module Check (S : State.S) = struct
    let value = S.Barrage.check Barrage.can_barrage
  end
  module Make (S : State.S) = struct
    module Bonus = Bonus.Make(S)
    module Fill = Dist.Fill(S.Dice)
    let trained, rest =
      S.Units.return Units.(filter Attr.barrage)
      |> Units.(split Attr.archer)
    let base = Power.base |> Bonus.brg_power
    let power coef units =
      Bonus.brg_coef coef *. Power.of_units units base
    let p_trained =
      power Barrage.trained_coefficient trained
    let p_rest =
      let coef = S.Barrage.return Barrage.coefficient in
      power coef rest
    let value =
      S.Enemy.return Units.(filter Attr.barraged)
      |> Fill.from (p_trained +. p_rest) base
      |> fst
  end
end

module Cyclops = struct
  type t = Units.t * Units.t
  module Apply (S : State.S) = struct
    let value (killed, rem) =
      S.Casualty.map (Units.combine killed);
      S.Units.set rem
  end
  module Check = Check.NoFog
  module Make (S : State.S) = struct
    module Bonus = Bonus.Make(S)
    module Fill = Dist.Fill(S.Dice)
    let cyclops = S.Enemy.return Units.(only Cyclops)
    let damage = Power.(artillery cyclops |> of_units cyclops)
    let defense = Power.base |> Bonus.siege_boost
    let value =
      Fill.from damage defense
      |> S.Units.return
  end
end

module Defeat = struct
  include Event.NoValue
  module Apply (S : State.S) = struct
    let value = S.Ended.set
  end
  module Check (S : State.S) = struct
    let value = S.Units.check Units.is_empty
  end
end

module Disease = struct
  type leader_died = bool
  type t = Units.t * leader_died
  let casualty = 0.1
  module Apply (S : State.S) = struct
    module LdrDied = Event.LdrDied(S)
    let value (died, ldr_died) =
      S.Units.map (Units.reduce died);
      if ldr_died then LdrDied.value 1
  end
  module Check (S : State.S) = struct
    let value = S.Mishap.check Mishap.(has Disease)
  end
  module Make (S : State.S) = struct
    module Fill = Units.Fill(S.Dice)
    module Roll = Leader.Roll(S.Dice)
    let units = S.Units.return Units.(filter Attr.infectable)
    let loss = Units.count_all units |> Number.portion casualty
    let died, _ = Fill.from loss units
    let value = died, S.Leader.return Roll.death
  end
end

module HitRun = struct
  type t = Units.t * Units.t
  module Apply (S : State.S) = struct
    let value (enemy, units) =
      S.Enemy.map (Units.reduce enemy);
      S.Units.map (Units.reduce units)
  end
  module Check (S : State.S) = struct
    let value = S.Barrage.check Barrage.can_hit_run
  end
  module Make = Combat.HitRun
end

module LeaderLvup = struct
  type t = Leader.t
  module Apply (S : State.S) = struct
    let value = S.Leader.set
  end
  module Check (S : State.S) = struct
    let value = S.Leader.check Leader.can_level_up
  end
  module Make (S : State.S) = struct
    let value = S.Leader.return Leader.level_up
  end
end

module Mangonel = struct
  type t = Units.t * Units.t
  module Apply (S : State.S) = struct
    let value (died, _) = S.Enemy.map (Units.reduce died)
  end
  module Check = Check.NoFog
  module Make (S : State.S) = struct
    module Bonus = Bonus.Make(S)
    module Fill = Dist.Fill(S.Dice)
    let mang = S.Units.return (Units.only Mangonel)
    let power = Bonus.artillery (Power.artillery mang)
    let damage = Power.of_units mang power
    let value =
      S.Enemy.return Units.(discard Attr.flying)
      |> Fill.from damage Power.base
  end
end

module Smite = struct
  type t = Units.t
  module Apply (S : State.S) = struct
    let value died = S.Enemy.map (Units.reduce died)
  end
  module Check (S : State.S) = struct
    let value = S.Deity.is Deity.Lerota
  end
  module Make (S : State.S) = struct
    module Bonus = Bonus.Make(S)
    module Fill = Dist.Fill(S.Dice)
    let cap = S.Dice.betweenf 5. 15. |> Bonus.smite
    let units = S.Enemy.return Units.(filter Attr.undead)
    let value = Fill.from cap Power.base units |> fst
  end
end
*)
