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
    let power' = Float.times count power
    let value = count, S.Enemy.return (Roll.from power')
  end
end

module Barraged = struct
  type t = Defs.count
  module Apply (S : State.S) = struct
    let value n = S.Enemy.map Units.(sub n Orc)
  end
  module Check (S : State.S) = struct
    let value = S.Barrage.check Barrage.is_chosen
  end
  module Make (S : State.S) = struct
    let clear = S.Weather.is Weather.Clear
    let trained = S.Barrage.check Barrage.is_trained
    let power = S.Units.return Units.barrage_power
    let base = if trained then 0.1 else 0.05
    let ratio = base |> Float.add_if clear 0.02
    let n = truncate (power *. ratio)
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
    let power' = Float.times count power
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
  let casualty = 0.1
  module Apply (S : State.S) = struct
    let value (units, died) =
      S.Units.map Units.(reduce units);
      if died then Leader.died |> S.Turn.return |> S.Leader.map
  end
  module Check (S : State.S) = struct
    let value = S.Mishap.check Mishap.(has Disease)
  end
  module Make (S : State.S) = struct
    module Fill = Units.FillCount(S.Dice)
    module Roll = Leader.Roll(S.Dice)
    let units = S.Units.return Units.(filter Attr.is_infectable)
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
    let boost = if S.Build.check Build.(is_ready Observatory) then 15 else 0
    let n = S.Dice.between 15 35 + boost
    let value = S.Enemy.return Units.(find n Skeleton)
  end
end
