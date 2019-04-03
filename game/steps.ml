type ('c, 'd) input =
  | Cond of 'c
  | Direct of 'd

type ('c, 'd, 'n) output =
  | Cond of 'c
  | Direct of 'd
  | Notify of 'n

type ('i, 'o) step =
  | Ask of 'i
  | Do of 'o
  | Go of ('i, 'o) step list
  | GoIf of ('o * ('i, 'o) step list)

module type Input = sig
  type cond and direct
  type t = (cond, direct) input
end

module type Output = sig
  type cond and direct and notify
  type t = (cond, direct, notify) output
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
    type cond = unit
    type direct = BuildSupply | Starting | Support
    type notify = unit
    type t = (cond, direct, notify) output
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
    type cond = Mercs
    type direct = Build | Nations
    type t = (cond, direct) input
  end
  module Output = struct
    type cond = Cavalry | Defeat | LeaderNew | Market | Starvation
    type direct = Blessing | BuildManp | BuildStatus | BuildSupply | Enemies | Support | Turn | Upkeep
    type notify = unit
    type t = (cond, direct, notify) output
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
      Do (Direct Output.Enemies);
      Do (Direct Output.Blessing);
      Do (Cond Output.Market);
      Ask (Direct Input.Nations);
      Do (Direct Output.Support);
      Ask (Direct Input.Build);
      Do (Direct Output.BuildSupply);
      Do (Cond Output.Cavalry);
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
    type cond = Barraged | Casualty | Defeat | Fort | LeaderDied | LeaderLvUp | Smite
    type direct = Victory
    type notify = Attack | NoAttack | NoEnemies
    type t = (cond, direct, notify) output
  end
  type t = (Input.t, Output.t) step
  let scout : t = Ask (Direct Input.Scout)
  let victory : t list =
    [ Do (Direct Output.Victory);
      Do (Cond Output.LeaderLvUp);
      scout
    ]
  let check_enemies : t =
    GoIf (Notify Output.NoEnemies, victory)
  let attack : t list =
    [ Do (Cond Output.Smite);
      check_enemies;
      Ask (Cond Input.Barrage);
      Do (Cond Output.Barraged);
      check_enemies;
      Do (Cond Output.Casualty);
      Do (Cond Output.Fort);
      Do (Cond Output.Defeat);
      Do (Cond Output.LeaderDied);
      Go victory
    ]
  let list : t list =
    [ GoIf (Notify Output.Attack, attack);
      Do (Notify Output.NoAttack);
      scout
    ]
end
