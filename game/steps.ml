type ('c, 'd) input =
  | Cond of 'c
  | Direct of 'd

type ('ch, 'co, 'd) output =
  | Check of 'ch
  | Cond of 'co
  | Direct of 'd

type ('i, 'o) step =
  | Ask of 'i
  | Do of 'o
  | Go of ('i, 'o) step list
  | GoIf of ('o * ('i, 'o) step list)
  | TryAsk of ('o * 'i)

module type Input = sig
  type cond and direct
  type t = (cond, direct) input
end

module type Output = sig
  type check and cond and direct
  type t = (check, cond, direct) output
end

module type S = sig
  module Input : Input
  module Output : Output
  type t = (Input.t, Output.t) step
  val list : t list
end

module Phase1 = struct
  module Input = struct
    type cond = unit
    type direct = Build | Deity | Leader | Nations | Scout
    type t = (cond, direct) input
  end
  module Output = struct
    type check = unit
    type cond = unit
    type direct = BuildSupply | Starting | Support
    type t = (check, cond, direct) output
  end
  type t = (Input.t, Output.t) step
  let list : t list =
    [ Ask (Direct Input.Deity);
      Ask (Direct Input.Leader);
      Do (Direct Output.Starting);
      Ask (Direct Input.Nations);
      Do (Direct Output.Support);
      Ask (Direct Input.Build);
      Do (Direct Output.BuildSupply);
      Ask (Direct Input.Scout)
    ]
end

module Phase2 = struct
  module Input = struct
    type cond = Mercs | Ranger | Templar | Trade
    type direct = Ballista | Build | Dervish | Nations
    type t = (cond, direct) input
  end
  module Output = struct
    type check = unit
    type cond = Cavalry | Defeat | LeaderNew | Market | Starvation
    type direct = Attack | Blessing | BuildManp | BuildStatus | BuildSupply | Support | Turn | Upkeep
    type t = (check, cond, direct) output
  end
  type t = (Input.t, Output.t) step
  let list : t list =
    [ Do (Direct Output.Turn);
      Do (Direct Output.BuildManp);
      Do (Direct Output.BuildStatus);
      Do (Cond Output.LeaderNew);
      Do (Direct Output.Upkeep);
      Do (Cond Output.Starvation);
      Do (Cond Output.Defeat);
      Do (Direct Output.Attack);
      Do (Direct Output.Blessing);
      Do (Cond Output.Market);
      Ask (Direct Input.Nations);
      Do (Direct Output.Support);
      Ask (Cond Input.Trade);
      Ask (Direct Input.Build);
      Do (Direct Output.BuildSupply);
      Ask (Direct Input.Ballista);
      Do (Cond Output.Cavalry);
      Ask (Direct Input.Dervish);
      Ask (Cond Input.Templar);
      Ask (Cond Input.Ranger);
      Ask (Cond Input.Mercs)
    ]
end

module Phase3 = struct
  module Input = struct
    type cond = Barrage
    type direct = Scout
    type t = (cond, direct) input
  end
  module Output = struct
    type check = Attack | LevelUp | NoAttack | NoEnemies
    type cond = Barraged | Defeat | Revive | Smite
    type direct = Ballista | CanBarrage | Combat | Victory
    type t = (check, cond, direct) output
  end
  type t = (Input.t, Output.t) step
  let scout : t = Ask (Direct Input.Scout)
  let victory : t list =
    [ Do (Direct Output.Victory);
      Do (Check Output.LevelUp);
      scout
    ]
  let check_enemies : t =
    GoIf (Check Output.NoEnemies, victory)
  let attack : t list =
    [ Do (Cond Output.Smite);
      check_enemies;
      Do (Direct Output.Ballista);
      check_enemies;
      Do (Direct Output.CanBarrage);
      Ask (Cond Input.Barrage);
      Do (Cond Output.Barraged);
      check_enemies;
      Do (Direct Output.Combat);
      Do (Cond Output.Revive);
      Do (Cond Output.Defeat);
      Go victory
    ]
  let list : t list =
    [ GoIf (Check Output.Attack, attack);
      Do (Check Output.NoAttack);
      scout
    ]
end
