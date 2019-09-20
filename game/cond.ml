module Ballista = struct
  type t = Defs.count * Units.t
  let power = 2.
  module Apply (S : State.S) = struct
    let value (_, enemies) = S.Enemy.map (Units.reduce enemies)
  end
  module Check = Check.NoFog
  module Make (S : State.S) = struct
    module Roll = Units.Fill(S.Dice)
    let count = S.Units.return Units.(count Ballista)
    let power' = Defs.to_power count power
    let value = count, S.Enemy.return (Roll.from power')
  end
end

module Barraged = struct
  type t = Defs.count
  module Apply (S : State.S) = struct
    let value n = S.Enemy.map Units.(sub n Orc)
  end
  module Check (S : State.S) = struct
    let value = S.Barraging.get ()
  end
  module Make (S : State.S) = struct
    let n = S.Units.return Units.barrage_power |> truncate
    let value = S.Enemy.return Units.(find n Orc)
  end
end

module Cyclops = struct
  type t = Defs.count * Units.t
  let power = 2.
  module Apply (S : State.S) = struct
    let value (_, loss) =
      S.Casualty.map (Units.combine loss);
      S.Units.map (Units.reduce loss)
  end
  module Check = Check.NoFog
  module Make (S : State.S) = struct
    module Roll = Units.Fill(S.Dice)
    let count = S.Enemy.return Units.(count Cyclops)
    let power' = Defs.to_power count power
    let value = count, S.Units.return (Roll.from power')
  end
end

module Defeat = struct
  include Event.NoValue
  module Apply (S : State.S) = struct
    let value = S.Ended.set
  end
  module Check (S : State.S) = struct
    let value = S.Units.empty ()
  end
end

module Disease = struct
  type leader_died = bool
  type t = Units.t * leader_died
  let chance = 0.05
  let min_count = 50
  let casualty = 0.1
  let penalty = 0.2
  let susceptible = Units.(discard Attr.is_siege)
  module Apply (S : State.S) = struct
    let value (units, died) =
      S.Disease.set penalty;
      S.Units.map Units.(reduce units);
      if died then Leader.died |> S.Turn.return |> S.Leader.map
  end
  module Check (S : State.S) = struct
    let _ = S.Disease.clear ()
    let count = S.Units.return susceptible |> Units.count_all
    let value = count >= min_count && S.Dice.chance chance
  end
  module Make (S : State.S) = struct
    module Fill = Units.FillCount(S.Dice)
    module Roll = Leader.Roll(S.Dice)
    let units = S.Units.return susceptible
    let loss = Units.count_all units |> Number.portion casualty
    let value =
      Fill.from loss units,
      S.Leader.return Roll.death
  end
end

module Revive = struct
  type t = Units.t
  module Apply (S : State.S) = struct
    let value revived = S.Units.map (Units.combine revived)
  end
  module Check (S : State.S) = struct
    let value = S.Units.check Units.(has Dervish)
  end
  module Make (S : State.S) = struct
    module Fill = Units.Fill(S.Dice)
    let pwr = S.Units.return Units.(power_of Dervish)
    let value =
      Units.(filter Attr.is_revivable)
      |> S.Casualty.return
      |> Fill.from pwr
  end
end

module Starvation = struct
  type t = Units.t
  module Apply (S : State.S) = struct
    let value units =
      S.Units.map Units.(reduce units);
      S.Supply.clear ()
  end
  module Check (S : State.S) = struct
    let value = S.Supply.ngv ()
  end
  module Make (S : State.S) = struct
    let cost = -S.Supply.get ()
    let value = S.Units.return (Units.starve cost)
  end
end

module Smite = struct
  type t = Defs.count
  module Apply (S : State.S) = struct
    let value n = S.Enemy.map Units.(sub n Skeleton)
  end
  module Check (S : State.S) = struct
    let value = S.Deity.is Deity.Lerota
      && S.Enemy.check Units.(has Skeleton)
  end
  module Make (S : State.S) = struct
    let boost = if S.Build.check Build.(ready Observatory) then 15 else 0
    let n = S.Dice.between 15 35 + boost
    let value = S.Enemy.return Units.(find n Skeleton)
  end
end
