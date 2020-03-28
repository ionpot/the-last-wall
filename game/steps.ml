type ('c, 'd) event =
  | Cond of 'c
  | Direct of 'd

module Label = struct
  type t = Combat | Upkeep
end

module Input = struct
  type direct = Ballista | BarrageTrain | Build | Deity | Dervish | Knight | Leader | Mangonel | Mercs | MercsEnd | Nations | Novice | Research | Scout | Sodistan | Xbowman
  type cond = Barracks | Barrage | Berserker | Harcher | LeaderNew | Marms | Ranger | Templar | Temple | Trade | Veteran | Volunteers
  type t = (cond, direct) event
end

module Output = struct
  type direct = Attack | Blessing | BuildManp | BuildStatus | BuildSupply | Cavalry | Combat | Facilities | Fear | FearEnd | Mishap | ResearchProgress | ResearchStatus | Revive | SiegeCombat | Starting | Starvation | Support | Turn | Upkeep | Victory
  type cond = Barraged | Defeat | Disease | HitRun | LeaderLvup | Mangonel | Smite
  type t = (cond, direct) event
  let last = Cond Defeat
end

type t =
  | Ask of Input.t
  | Do of Output.t
  | GoTo of Label.t
  | Mark of Label.t

let ls : t list =
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

  ; Mark Label.Upkeep
  ; Do (Direct Output.Turn)
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
  ; Ask (Cond Input.Berserker)
  ; Ask (Direct Input.Ballista)
  ; Ask (Direct Input.Mangonel)
  ; Do (Direct Output.Cavalry)
  ; Ask (Cond Input.Harcher)
  ; Ask (Cond Input.Marms)
  ; Ask (Direct Input.Knight)
  ; Ask (Cond Input.Veteran)
  ; Ask (Direct Input.Xbowman)
  ; Ask (Direct Input.Novice)
  ; Ask (Direct Input.Dervish)
  ; Ask (Cond Input.Templar)
  ; Ask (Cond Input.Ranger)
  ; Ask (Direct Input.MercsEnd)
  ; Ask (Direct Input.Mercs)
  ; Ask (Direct Input.BarrageTrain)

  ; Mark Label.Combat
  ; Do (Cond Output.Smite)
  ; Do (Direct Output.SiegeCombat)
  ; Do (Cond Output.Defeat)
  ; Ask (Cond Input.Barrage)
  ; Do (Cond Output.Barraged)
  ; Do (Cond Output.Mangonel)
  ; Do (Cond Output.HitRun)
  ; Do (Direct Output.Fear)
  ; Do (Direct Output.Combat)
  ; Do (Direct Output.Revive)
  ; Do (Cond Output.Defeat)
  ; Do (Direct Output.Victory)
  ; Do (Cond Output.LeaderLvup)
  ; Ask (Direct Input.Scout)
  ; GoTo Label.Upkeep
  ]
