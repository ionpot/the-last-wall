module Ballista = Recruit.Event(struct
  let action = Recruit.Train
  let kind = Units.Ballista
  let pool = None
  module Cap = Recruit.NoCap
end)

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

module Berserker = Recruit.Event(struct
  let action = Recruit.Promote
  let kind = Units.Berserker
  let pool = Some (Recruit.From Pool.Arena)
  module Cap = Recruit.NoCap
end)

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

module Dervish = Recruit.Event(struct
  let action = Recruit.Add
  let kind = Units.Dervish
  let pool = Some (Recruit.To Pool.Dervish)
  module Cap (S : State.S) = struct
    let range =
      if S.Build.check Build.(is_ready Guesthouse)
      then 3, 12 else 2, 8
    let value = Some (S.Dice.range range)
  end
end)

module Knight = Recruit.Event(struct
  let action = Recruit.Promote
  let kind = Units.Knight
  let pool = None
  module Cap = Recruit.NoCap
end)

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
  include Recruit.Event(struct
    let action = Recruit.Add
    let kind = Units.Merc
    let pool = None
    module Cap (S : State.S) = struct
      let value = Some (S.Dice.between 10 20)
    end
  end)
  module Check (S : State.S) = struct
    let value = S.Build.check Build.(is_ready Tavern)
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
  include Recruit.Event(struct
    let action = Recruit.Promote
    let kind = Units.Ranger
    let pool = Some (Recruit.Exclude Pool.Dervish)
    module Cap = Recruit.NoCap
  end)
  module Check (S : State.S) = struct
    let value = S.Deity.is Deity.Sitera
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
  include Recruit.Event(struct
    let action = Recruit.Promote
    let kind = Units.Templar
    let pool = Some (Recruit.Exclude Pool.Dervish)
    module Cap = Recruit.NoCap
  end)
  module Check = Check.Not(Ranger.Check)
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
      S.Nation.map (Nation.set_trade trade);
      if S.Turn.is 0 then set_chance trade
  end
  module Check (S : State.S) = struct
    let value =
      S.Build.check Build.(is_complete Trade)
      && S.Nation.check Nation.no_trade
  end
  module Make (S : State.S) = struct
    let value = None
  end
end

module Veteran = struct
  include Recruit.Event(struct
    let action = Recruit.Promote
    let kind = Units.Veteran
    let pool = None
    module Cap = Recruit.NoCap
  end)
  module Check (S : State.S) = struct
    let value = S.Build.check Build.(is_ready Barracks)
  end
end

module Volunteers = struct
  module Range = Range.Int
  type t = Defs.count
  let kind = Units.Men
  module Apply (S : State.S) = struct
    let value n = S.Units.map (Units.add n kind)
  end
  module Check (S : State.S) = struct
    let value = S.Build.check Build.(is_ready Tavern)
  end
  module Make (S : State.S) = struct
    let noble = S.Leader.check Leader.(is_living Aristocrat)
    let cha = S.Leader.return Leader.cha_mod_of
    let n = Number.add_if noble cha 3
    let value = Range.times n (1, 3) |> S.Dice.range
  end
end
