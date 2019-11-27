module Ballista = struct
  type t = Defs.count * Units.t * Units.t
  let kind = Units.Ballista
  let power = 2.
  module Apply (S : State.S) = struct
    let value (_, _, remaining) = S.Enemy.set remaining
  end
  module Check = Check.NoFog
  module Make (S : State.S) = struct
    module Fill = Dist.Fill(S.Dice)
    let count = S.Units.return (Units.count kind)
    let eng = S.Leader.check Leader.(is_living Engineer)
    let power = Float.add_if eng 1. power
    let damage = Float.times count power
    let base = S.Bonus.return Power.base
    let killed, rem =
      Fill.from damage base
      |> S.Enemy.return
    let value = count, killed, rem
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
    module Fill = Dist.Fill(S.Dice)
    let units = S.Units.return Units.(filter Attr.can_barrage)
    let base = Power.barrage
    let p =
      Barrage.coefficient
      |> S.Bonus.return
      |> S.Barrage.return
      |> ( *. ) (Power.of_units units base)
    let value =
      S.Enemy.return Units.(filter Attr.can_barraged)
      |> Fill.from p base |> fst
  end
end

module Cyclops = struct
  type t = Defs.count * Units.t * Units.t
  let power = 2.
  module Apply (S : State.S) = struct
    let value (_, killed, rem) =
      S.Casualty.map (Units.combine killed);
      S.Units.set rem
  end
  module Check = Check.NoFog
  module Make (S : State.S) = struct
    module Fill = Dist.Fill(S.Dice)
    let count = S.Enemy.return Units.(count Cyclops)
    let damage = Float.times count power
    let base = S.Bonus.return Power.base
    let killed, rem =
      Fill.from damage base
      |> S.Units.return
    let value = count, killed, rem
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
    let units = S.Units.return Units.(filter Attr.is_infectable)
    let loss = Units.count_all units |> Number.portion casualty
    let died, _ = Fill.from loss units
    let value = died, S.Leader.return Roll.death
  end
end

module HitRun = struct
  type t = Units.t * Units.t
  let hit_back_chance = 0.05
  module Apply (S : State.S) = struct
    let value (enemy, units) =
      S.Enemy.map (Units.reduce enemy);
      S.Units.map (Units.reduce units)
  end
  module Check (S : State.S) = struct
    let value = S.Barrage.check Barrage.can_hit_run
  end
  module Make (S : State.S) = struct
    module Fill = Dist.Fill(S.Dice)
    let base = S.Bonus.return Power.base
    let fill p u = Fill.from p base u |> fst
    let units = S.Units.return Units.(filter Attr.can_hit_run)
    let power = Power.of_units units base
    let c =
      Barrage.coefficient
      |> S.Bonus.return
      |> S.Barrage.return
    let enemy = S.Enemy.get ()
    let epower = Power.of_units enemy base
    let target = Units.(filter Attr.can_barraged) enemy
    let value =
      fill (power *. c) target,
      if S.Dice.chance hit_back_chance
      then fill (epower *. 0.1) units
      else Units.empty
  end
end

module Smite = struct
  type t = Units.t
  module Apply (S : State.S) = struct
    let value died =
      S.Enemy.map (Units.reduce died)
  end
  module Check (S : State.S) = struct
    let value = S.Deity.is Deity.Lerota
  end
  module Make (S : State.S) = struct
    module Fill = Dist.Fill(S.Dice)
    let obser = S.Build.check Build.(is_ready Observatory)
    let cap = S.Dice.betweenf 5. 15. |> Float.add_if obser 10.
    let base = S.Bonus.return Power.base
    let units = S.Enemy.return Units.(filter Attr.is_undead)
    let value = Fill.from cap base units |> fst
  end
end
