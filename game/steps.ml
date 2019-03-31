type ('c, 'd) input =
  | Cond of 'c
  | Direct of 'd

type ('c, 'd, 'n) output =
  | Cond of 'c
  | Direct of 'd
  | Notify of 'n

type ('i, 'o) step =
  | Ask of 'i
  | Branch of ('o * ('i, 'o) step list)
  | Do of 'o
  | Either of ('o * 'o)

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

(*module Phase2 = struct
  type cond = Cavalry | Defeat | LeaderNew | Market | Starvation
  type direct = Blessing | BuildManp | BuildStatus | BuildSupply | Enemies | Support | Turn | Upkeep
  type input = Build | Mercs | Nations
  type notify = unit

  type event = (cond, direct, input, notify) etype
  type t = event step list

  let list =
    [ Do (Direct Turn);
      Do (Direct BuildManp);
      Do (Direct BuildStatus);
      Do (Cond LeaderNew);
      Do (Direct Upkeep);
      Do (Cond Starvation);
      Do (Cond Defeat);
      Do (Direct Enemies);
      Do (Direct Blessing);
      Do (Cond Market);
      Do (Input Nations);
      Do (Direct Support);
      Do (Input Build);
      Do (Direct BuildSupply);
      Do (Cond Cavalry);
      Do (Input Mercs)
    ]
end

module Phase3 = struct
  type cond = Barraged | Casualty | Defeat | Fort | LeaderDied | LeaderLvUp | Smite
  type direct = Victory
  type input = Barrage | Scout
  type notify = Attack | NoAttack

  type event = (cond, direct, input, notify) etype
  type t = event step list

  let attack =
    [ Do (Cond Smite);
      Do (Input Barrage);
      Do (Cond Barraged);
      Do (Cond Casualty);
      Do (Cond Fort);
      Either (Cond Defeat, Direct Victory);
      Either (Cond LeaderDied, Cond LeaderLvUp);
      Do (Input Scout)
    ]

  let list =
    [ Branch (Notify Attack, attack);
      Do (Notify NoAttack);
      Do (Input Scout)
    ]
end*)
