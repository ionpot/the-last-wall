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

module Mangonel = struct
  type t = Units.t * Units.t
  module Apply (S : State.S) = struct
    let value (_, remaining) = S.Enemy.set remaining
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
