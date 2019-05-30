module Barraged = struct
  type t = Defs.count
  module Apply (S : State.S) = struct
    let value n = S.Enemy.map Units.(sub n Orc)
  end
  module Check (S : State.S) = struct
    let value = S.Barraging.get ()
  end
  module Make (S : State.S) = struct
    let n = S.Units.return Units.(count Men) / 20
    let value = S.Enemy.return Units.(find n Orc)
  end
end

module Cavalry = struct
  type t = Defs.count
  module Apply (S : State.S) = struct
    let value n =
      S.Units.map Units.(add n Cavalry);
      S.Units.map Units.(sub n Men);
      S.Supply.sub n
  end
  module Check = Cavalry.Check
  module Make = Cavalry.Make
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

module LeaderNew = struct
  type t = Leader.t
  module Apply (S : State.S) = struct
    let value = S.Leader.set
  end
  module Check (S : State.S) = struct
    let can_respawn = S.Turn.return Leader.can_respawn
    let value = S.Leader.check can_respawn
  end
  module Make (S : State.S) = struct
    module Roll = Leader.Roll(S.Dice)
    let value = Roll.random ()
  end
end

module Market = struct
  type t = Defs.supply
  module Apply (S : State.S) = struct
    let value = S.Supply.add
  end
  module Check (S : State.S) = struct
    let value = S.Build.check Build.(ready Market)
  end
  module Make (S : State.S) = struct
    let value = S.Dice.between 15 45
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
      Units.(rm Cavalry)
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
