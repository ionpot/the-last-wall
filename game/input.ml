module Ballista = struct
  type t = Defs.count * Defs.count
  let kind = Units.Ballista
  module Apply (S : State.S) = struct
    module Recruit = Recruit.With(S)
    let value (n, _) =
      Recruit.sub_cost kind n;
      let eng = S.Leader.check Leader.(is_living Engineer) in
      let normal, fast = if eng then 0, n else n, 0 in
      let n' = S.Ballista.get () + fast in
      S.Units.map (Units.add n' kind);
      S.Ballista.set normal
  end
  module Make (S : State.S) = struct
    module Recruit = Recruit.With(S)
    let have = S.Units.return (Units.count kind)
    let total = have + S.Ballista.get ()
    let cap = S.Build.return Build.ballista_cap
    let avlb = Recruit.affordable kind (cap - total)
    let value = avlb, total
  end
end

module Barracks = struct
  type t = Nation.kind option
  module Apply (S : State.S) = struct
    let value choice =
      S.Nation.map (Nation.set_barracks choice)
  end
  module Check (S : State.S) = struct
    let value =
      S.Build.check Build.(is_complete Barracks)
      && S.Nation.check Nation.no_barracks
  end
  module Make (S : State.S) = struct
    let value = None
  end
end

module BarrageTrain = struct
  type cost = Defs.supply
  type t = bool * cost
  module Apply (S : State.S) = struct
    let value (ok, cost) =
      S.Barrage.map (Barrage.set_trained ok);
      if ok then S.Supply.sub cost
  end
  module Make (S : State.S) = struct
    let units = S.Units.return Units.(filter Attr.can_barrage)
    let base = S.Bonus.return Power.base
    let power = Power.of_units units base
    let cost = (power *. 0.05) |> ceil |> truncate
    let value = S.Supply.has cost, cost
  end
end

module Barrage = struct
  type t = bool * Barrage.status
  module Apply (S : State.S) = struct
    let value (ok, status) =
      let ok' = status = Barrage.Available && ok in
      S.Barrage.map (Barrage.set_choice ok');
      S.Bonus.map Bonus.(set Barrage ok')
  end
  module Make (S : State.S) = struct
    let value = false,
      if S.Leader.check Leader.is_dead
      then Barrage.(Disabled Leader)
      else if S.Weather.check Weather.is_bad
      then Barrage.(Disabled Weather)
      else Barrage.Available
  end
end

module Berserker = struct
  type t = Defs.count
  let kind = Units.Berserker
  module Apply (S : State.S) = struct
    module Recruit = Recruit.With(S)
    let value = Recruit.promote kind
  end
  module Make (S : State.S) = struct
    module Recruit = Recruit.With(S)
    let arena = S.Arena.get ()
    let n = Power.translate Units.Men kind arena Power.empty
    let cap = Recruit.(Missing.arena () |> affordable kind)
    let value = min n cap
  end
end

module BuildAvlb = struct
  type chosen = Build.kind list
  type t = chosen * Build.cost_map
  module Bonus = Build.Bonus
  module Apply (S : State.S) = struct
    let value (chosen, costs) = S.Build.map (Build.start chosen costs)
  end
  module Make (S : State.S) = struct
    let has_engrs = S.Build.check Build.(is_ready Engrs)
    let elanis = S.Deity.is Deity.Elanis
    let costs = S.Build.return Build.cost_map
      |> Bonus.to_cost_if has_engrs
          (Bonus.ToAll, Resource.Bonus.(Sub (Sup 0.1)))
      |> Bonus.to_cost_if elanis
          (Bonus.To Build.Stable, Resource.Bonus.(Sub (Both 0.2)))
    let value = [], costs
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
  let kind = Units.Dervish
  module Apply (S : State.S) = struct
    module Recruit = Recruit.With(S)
    let value n =
      Recruit.promote kind n;
      S.Dervish.set n
  end
  module Make (S : State.S) = struct
    module Recruit = Recruit.With(S)
    let cap = Recruit.(Missing.temple () |> affordable kind)
    let a, b = Recruit.dervish_range ()
    let value = S.Dice.between_try a (min b cap)
  end
end

module Knight = struct
  type t = Defs.count
  let kind = Units.Knight
  module Apply (S : State.S) = struct
    module Recruit = Recruit.With(S)
    let value = Recruit.promote kind
  end
  module Make (S : State.S) = struct
    module Recruit = Recruit.With(S)
    let value = Recruit.promotable kind
  end
end

module LeaderKind = struct
  type t = Leader.kind
  module Apply (S : State.S) = struct
    module Roll = Leader.Roll(S.Dice)
    let value kind = S.Leader.set (Roll.from kind)
  end
  module Make (S : State.S) = struct
    let value = Leader.(kind_of empty)
  end
end

module LeaderNew = struct
  type leader = Leader.t * bool
  type t = leader * leader
  let cost_of = Leader.level_of
  module Apply (S : State.S) = struct
    let set ldr =
      S.Supply.sub (cost_of ldr);
      S.Leader.set ldr
    let value = function
      | (ldr, true), _
      | _, (ldr, true) -> set ldr
      | _ -> ()
  end
  module Check (S : State.S) = struct
    let value = S.Build.check Build.(is_ready Tavern)
      && S.Leader.check (S.Turn.return Leader.can_respawn)
  end
  module Make (S : State.S) = struct
    module Roll = Leader.Roll(S.Dice)
    let a, b = Roll.pair ()
    let check ldr = S.Supply.has (cost_of ldr)
    let value = (a, check a), (b, check b)
  end
end

module Mercs = struct
  type t = Defs.count
  let kind = Units.Merc
  module Apply (S : State.S) = struct
    module Recruit = Recruit.With(S)
    let value = Recruit.promote kind
  end
  module Check (S : State.S) = struct
    let value = S.Build.check Build.(is_ready Tavern)
  end
  module Make (S : State.S) = struct
    module Recruit = Recruit.With(S)
    let cap = S.Dice.between 10 20
    let value = Recruit.supply_limit kind cap
  end
end

module Nations = struct
  type t = Nation.Set.t
  module Apply (S : State.S) = struct
    let value x = S.Nation.map (Nation.set_chosen x)
  end
  module Make (S : State.S) = struct
    let value = S.Nation.return Nation.chosen
  end
end

module Ranger = struct
  type t = Defs.count
  let kind = Units.Ranger
  module Apply (S : State.S) = struct
    module Recruit = Recruit.With(S)
    let value = Recruit.promote kind
  end
  module Check (S : State.S) = struct
    let value = S.Deity.is Deity.Sitera
  end
  module Make (S : State.S) = struct
    module Recruit = Recruit.With(S)
    let value = Recruit.promotable kind
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

module Sodistan = struct
  type t = Defs.supply
  module Apply (S : State.S) = struct
    let value t =
      S.Units.map Units.(sub (t * 2) Men);
      S.Supply.add t
  end
  module Make (S : State.S) = struct
    module Check = Support.Check(S)
    let mnp = Check.traded_mnp Nation.Sodistan
    let men = S.Units.return Units.(count Men)
    let value = min mnp men / 2
  end
end

module Templar = struct
  type t = Defs.count
  let kind = Units.Templar
  module Apply (S : State.S) = struct
    module Recruit = Recruit.With(S)
    let value = Recruit.promote kind
  end
  module Check (S : State.S) = struct
    let value = not (S.Deity.is Deity.Sitera)
  end
  module Make (S : State.S) = struct
    module Recruit = Recruit.With(S)
    let value = Recruit.promotable kind
  end
end

module Trade = struct
  type t = Nation.kind option
  module Apply (S : State.S) = struct
    let set_chance = function
      | Some kind ->
          Nation.(Chance.set_trading kind |> map_chances)
          |> S.Nation.map
      | None -> ()
    let value trade =
      S.Build.map (Build.set_trade trade);
      if S.Turn.is 0 then set_chance trade
  end
  module Check (S : State.S) = struct
    let value = S.Build.check Build.need_trade
  end
  module Make (S : State.S) = struct
    let value = None
  end
end

module Veteran = struct
  type t = Defs.count
  let kind = Units.Veteran
  module Apply (S : State.S) = struct
    module Recruit = Recruit.With(S)
    let value = Recruit.promote kind
  end
  module Check (S : State.S) = struct
    let value = S.Build.check Build.(is_ready Barracks)
  end
  module Make (S : State.S) = struct
    module Recruit = Recruit.With(S)
    let value = Recruit.promotable kind
  end
end

module Volunteers = struct
  module Range = Range.Int
  type t = Defs.count
  let kind = Units.Men
  let base = 3, 9
  module Apply (S : State.S) = struct
    let value n = S.Units.map (Units.add n kind)
  end
  module Check (S : State.S) = struct
    let value = S.Build.check Build.(is_ready Tavern)
  end
  module Make (S : State.S) = struct
    let noble = S.Leader.check Leader.(is_living Aristocrat)
    let cha = S.Leader.return Leader.cha_mod_of
    let bonus = Range.times cha (1, 3)
    let value = Range.combine_if noble bonus base |> S.Dice.range
  end
end
