type ('c, 'd, 'i, 'n) etype =
  | Cond of 'c
  | Direct of 'd
  | Input of 'i
  | Notify of 'n

type 'a step =
  | Do of 'a
  | Branch of ('a * 'a step list)
  | Either of ('a * 'a)

module type S = sig
  type cond and direct and input and notify
  type event = (cond, direct, input, notify) etype
  type t = event step list
  val list : t
end

module Phase1 = struct
  type cond = BuildSupply
  type direct = Starting | Support
  type input = Build | Nations | Scout
  type notify = unit

  type event = (cond, direct, input, notify) etype
  type t = event step list

  let list =
    [ Do (Direct Starting);
      Do (Input Nations);
      Do (Direct Support);
      Do (Input Build);
      Do (Cond BuildSupply);
      Do (Input Scout)
    ]
end

module Phase2 = struct
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
end
