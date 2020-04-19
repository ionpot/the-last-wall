module DeityChoice = struct
  type t = Deity.t
  let apply = State.deity_set
  let make s = Deity.empty
end

module LeaderChoice = struct
  type t = Leader.kind
  let apply t = State.leader_set (Leader.Roll.from t)
  let make s = Leader.(kind_of empty)
end

(*
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
    let units =
      S.Units.return Units.(filter Attr.barrage)
      |> Units.(discard Attr.archer)
    let power = Units.power_of units
    let cost = (power *. 0.05) |> ceil |> truncate
    let value = S.Supply.has cost, cost
  end
end

module Barrage = struct
  type t = bool * Barrage.status
  module Apply (S : State.S) = struct
    let value (ok, status) =
      S.Barrage.return (Barrage.set_choice ok)
      |> Barrage.set_status status
      |> S.Barrage.set
  end
  module Make (S : State.S) = struct
    let value = false,
      if S.Leader.check Leader.is_dead
      then Barrage.(Disabled Leader)
      else if S.Weather.check Weather.is_bad
      then Barrage.(Disabled Weather)
      else if S.Units.check Units.(has_any Attr.barrage)
      then Barrage.Available
      else Barrage.(Disabled Archers)
  end
end

module BuildAvlb = struct
  type t = Build.kind list * Build.cost_map
  module Apply (S : State.S) = struct
    let f cmap kind = S.Build.map (Build.start kind cmap)
    let value (ls, cost_map) = List.iter (f cost_map) ls
  end
  module Make (S : State.S) = struct
    module Bonus = Bonus.Make(S)
    let value = [],
      S.Build.return Build.cost_map
      |> Build.Map.mapi Bonus.build_cost
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

module MercsEnd = struct
  type t = Defs.count
  module Apply (S : State.S) = struct
    let value n = S.Units.map Units.(sub n Merc)
  end
  module Make (S : State.S) = struct
    let value = S.Units.return Units.(count Merc)
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

module Research = struct
  type available = Research.Set.t
  type start = Research.kind list
  type t = start * available
  module Apply (S : State.S) = struct
    let value (s, _) =
      Research.start s
      |> S.Research.map
  end
  module Make (S : State.S) = struct
    let arena = S.Build.check Build.(is_ready Arena)
    let barracks = S.Build.check Build.(is_ready Barracks)
    let lerota = S.Deity.is Deity.Lerota
    let sawmill = S.Build.check Build.(is_ready Sawmill)
    let sitera = S.Deity.is Deity.Sitera
    let stable = S.Build.check Build.(is_ready Stable)
    let temple = S.Build.check Build.(is_ready Temple)
    let value = [],
      S.Research.get ()
      |> Research.(unlock AnimalTotems) (arena && sitera && temple)
      |> Research.(unlock BlackArmy) (lerota && temple)
      |> Research.(unlock CompositeBows) (barracks && sawmill && stable)
      |> Research.available
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
    let mnp = S.Nation.return Nation.(traded_mnp Sodistan)
    let men = S.Units.return Units.(count Men)
    let value = min mnp men / 2
  end
end

module Temple = struct
  type t = Defs.count
  module Apply (S : State.S) = struct
    let value n = S.Units.map Units.(add n Men)
  end
  module Check (S : State.S) = struct
    let value = S.Build.check Build.(is_ready Temple)
  end
  module Make (S : State.S) = struct
    module Bonus = Bonus.Make(S)
    let n = Bonus.temple_men 3
    let value = Range.Int.times n (1, 4) |> S.Dice.range
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
    module Bonus = Bonus.Make(S)
    let n = Bonus.volunteers 3
    let value = Range.times n (1, 3) |> S.Dice.range
  end
end
*)
