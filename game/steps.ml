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
  | Shuffle of (('i, 'o) step list * ('i, 'o) step list)
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
    type cond = Trade | Volunteers
    type direct = Ballista | Build | Deity | Knight | Leader | Nations | Scout | Sodistan
    type t = (cond, direct) input
  end
  module Output = struct
    type check = unit
    type cond = unit
    type direct = BuildSupply | Cavalry | Facilities | Starting | Support
    type t = (check, cond, direct) output
  end
  type t = (Input.t, Output.t) step
  let list : t list =
    [ Ask (Direct Input.Deity)
    ; Ask (Direct Input.Leader)
    ; Do (Direct Output.Starting)
    ; Ask (Cond Input.Trade)
    ; Do (Direct Output.Facilities)
    ; Ask (Direct Input.Nations)
    ; Do (Direct Output.Support)
    ; Ask (Direct Input.Sodistan)
    ; Ask (Cond Input.Volunteers)
    ; Ask (Direct Input.Build)
    ; Do (Direct Output.BuildSupply)
    ; Ask (Direct Input.Ballista)
    ; Do (Direct Output.Cavalry)
    ; Ask (Direct Input.Knight)
    ; Ask (Direct Input.Scout)
    ]
end

module Phase2 = struct
  module Input = struct
    type cond = Barracks | Harcher | LeaderNew | Ranger | Templar | Temple | Trade | Veteran | Volunteers
    type direct = Ballista | BarrageTrain | Berserker | Build | Dervish | Knight | Mercs | Nations | Novice | Research | Sodistan
    type t = (cond, direct) input
  end
  module Output = struct
    type check = unit
    type cond = Defeat | Disease
    type direct = Attack | Blessing | BuildManp | BuildStatus | BuildSupply | Cavalry | Facilities | FearEnd | Mishap | ResearchProgress | ResearchStatus | Starvation | Support | Turn | Upkeep
    type t = (check, cond, direct) output
  end
  type t = (Input.t, Output.t) step
  let list : t list =
    [ Do (Direct Output.Turn)
    ; Do (Direct Output.FearEnd)
    ; Do (Direct Output.BuildManp)
    ; Do (Direct Output.BuildStatus)
    ; Do (Direct Output.ResearchStatus)
    ; Ask (Cond Input.LeaderNew)
    ; Do (Direct Output.Upkeep)
    ; Do (Direct Output.Starvation)
    ; Do (Cond Output.Defeat)
    ; Do (Direct Output.Mishap)
    ; Do (Cond Output.Disease)
    ; Do (Direct Output.Attack)
    ; Do (Direct Output.Blessing)
    ; Do (Direct Output.Facilities)
    ; Ask (Direct Input.Nations)
    ; Do (Direct Output.Support)
    ; Ask (Direct Input.Sodistan)
    ; Ask (Cond Input.Volunteers)
    ; Ask (Cond Input.Temple)
    ; Ask (Cond Input.Barracks)
    ; Ask (Cond Input.Trade)
    ; Ask (Direct Input.Research)
    ; Do (Direct Output.ResearchProgress)
    ; Ask (Direct Input.Build)
    ; Do (Direct Output.BuildSupply)
    ; Ask (Direct Input.Berserker)
    ; Ask (Direct Input.Ballista)
    ; Do (Direct Output.Cavalry)
    ; Ask (Cond Input.Harcher)
    ; Ask (Direct Input.Knight)
    ; Ask (Cond Input.Veteran)
    ; Ask (Direct Input.Novice)
    ; Ask (Direct Input.Dervish)
    ; Ask (Cond Input.Templar)
    ; Ask (Cond Input.Ranger)
    ; Ask (Direct Input.Mercs)
    ; Ask (Direct Input.BarrageTrain)
    ]
end

module Phase3 = struct
  module Input = struct
    type cond = unit
    type direct = Barrage | Scout
    type t = (cond, direct) input
  end
  module Output = struct
    type check = Attack | LevelUp | NoAttack | NoEnemies
    type cond = Ballista | Barraged | Cyclops | Defeat | HitRun | Smite
    type direct = Combat | Fear | Revive | Victory
    type t = (check, cond, direct) output
  end
  type t = (Input.t, Output.t) step
  let scout : t = Ask (Direct Input.Scout)
  let victory : t list =
    [ Do (Direct Output.Victory)
    ; Do (Check Output.LevelUp)
    ; scout
    ]
  let check_enemies : t =
    GoIf (Check Output.NoEnemies, victory)
  let attack : t list =
    [ Do (Cond Output.Smite)
    ; check_enemies
    ; Shuffle (
      [ Do (Cond Output.Ballista)
      ; check_enemies
      ],
      [ Do (Cond Output.Cyclops)
      ; Do (Cond Output.Defeat)
      ])
    ; Ask (Direct Input.Barrage)
    ; Do (Cond Output.Barraged)
    ; Do (Cond Output.HitRun)
    ; check_enemies
    ; Do (Direct Output.Fear)
    ; Do (Direct Output.Combat)
    ; Do (Direct Output.Revive)
    ; Do (Cond Output.Defeat)
    ; Go victory
    ]
  let list : t list =
    [ GoIf (Check Output.Attack, attack)
    ; Do (Check Output.NoAttack)
    ; scout
    ]
end
