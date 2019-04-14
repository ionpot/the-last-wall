module BadWeather = struct
  type t = Weather.t
  module Apply (S : State.S) = struct
    let value _ = S.Barraging.clear ()
  end
  module Check (S : State.S) = struct
    let value = S.Weather.check Weather.is_bad
  end
  module Make (S : State.S) = struct
    let value = S.Weather.get ()
  end
end

module Barraged = struct
  type t = Defs.count
  module Apply (S : State.S) = struct
    let value count = S.Enemy.map (Enemy.reduce count Orc)
  end
  module Check (S : State.S) = struct
    let value = S.Barraging.get ()
  end
  module Make (S : State.S) = struct
    let count = S.Men.get () / 20
    let value = S.Enemy.return Enemy.(find count Orc)
  end
end

module Cavalry = struct
  type t = Defs.count
  module Apply (S : State.S) = struct
    let value =
      Listx.apply_to [S.Cavalry.add; S.Men.sub; S.Supply.sub]
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
    let value =
      S.Men.zero () && S.Cavalry.zero ()
  end
end

module LevelUp = struct
  type t = Leader.t
  module Apply (S : State.S) = struct
    let value = S.Leader.set
  end
  module Check (S : State.S) = struct
    let value = S.Leader.check Leader.can_lvup
  end
  module Make (S : State.S) = struct
    let value = S.Leader.return Leader.lvup
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
    let value = Leader.random ()
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
    let value = Dice.between 15 45
  end
end

module Starvation = struct
  type t = Defs.manpower * Defs.manpower
  module Apply (S : State.S) = struct
    let value (men, cav) =
      S.Men.sub men;
      S.Cavalry.sub cav;
      S.Supply.clear ()
  end
  module Check (S : State.S) = struct
    let value = S.Supply.ngv ()
  end
  module Make = Upkeep.Starvation
end

module Smite = struct
  type t = Defs.count
  module Apply (S : State.S) = struct
    let value count = S.Enemy.map (Enemy.reduce count Skeleton)
  end
  module Check (S : State.S) = struct
    let value = S.Deity.is Deity.Lerota
  end
  module Make (S : State.S) = struct
    let boost = if S.Build.check Build.(ready Temple) then 15 else 0
    let count = Dice.between 15 35 + boost
    let value = S.Enemy.return Enemy.(find count Skeleton)
  end
end