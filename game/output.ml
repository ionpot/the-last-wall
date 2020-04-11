open Output_event

type kind = unit
(*
  | Attack of Attack.t
  | Barraged of Barraged.t
  | Blessing of Blessing.t
  | BuildManp of BuildManp.t
  | BuildStatus of BuildStatus.t
  | BuildSupply of BuildSupply.t
  | Cavalry of Cavalry.t
  | Combat of Combat.t
  | Defeat of Defeat.t
  | Disease of Disease.t
  | Facilities of Facilities.t
  | Fear of Fear.t
  | FearEnd of FearEnd.t
  | HitRun of HitRun.t
  | LeaderLvup of LeaderLvup.t
  | Mangonel of Mangonel.t
  | Mishap of Mishap.t
  | ResearchProgress of ResearchProgress.t
  | ResearchStatus of ResearchStatus.t
  | Revive of Revive.t
  | SiegeCombat of SiegeCombat.t
  | Smite of Smite.t
  | Starting of Starting.t
  | Support of Support.t
  | Turn of Turn.t
  | Upkeep of Upkeep.t
  | Victory of Victory.t
*)
type 'a convert = 'a -> kind
type 'a cond = 'a Event.cond * 'a convert
type 'a direct = 'a Event.direct * 'a convert

let of_direct : Steps.Output.direct -> 'a direct =
  let module Direct = Steps.Output in
  function
(*
  | Direct.Attack -> (module Attack), (fun x -> Attack x)
  | Direct.Blessing -> (module Blessing), (fun x -> Blessing x)
  | Direct.BuildManp -> (module BuildManp), (fun x -> BuildManp x)
  | Direct.BuildStatus -> (module BuildStatus), (fun x -> BuildStatus x)
  | Direct.BuildSupply -> (module BuildSupply), (fun x -> BuildSupply x)
  | Direct.Cavalry -> (module Cavalry), (fun x -> Cavalry x)
  | Direct.Combat -> (module Combat), (fun x -> Combat x)
  | Direct.Facilities -> (module Facilities), (fun x -> Facilities x)
  | Direct.Fear -> (module Fear), (fun x -> Fear x)
  | Direct.FearEnd -> (module FearEnd), (fun x -> FearEnd x)
  | Direct.Mishap -> (module Mishap), (fun x -> Mishap x)
  | Direct.ResearchProgress -> (module ResearchProgress), (fun x -> ResearchProgress x)
  | Direct.ResearchStatus -> (module ResearchStatus), (fun x -> ResearchStatus x)
  | Direct.Revive -> (module Revive), (fun x -> Revive x)
  | Direct.SiegeCombat -> (module SiegeCombat), (fun x -> SiegeCombat x)
  | Direct.Starting -> (module Starting), (fun x -> Starting x)
  | Direct.Support -> (module Support), (fun x -> Support x)
  | Direct.Turn -> (module Turn), (fun x -> Turn x)
  | Direct.Upkeep -> (module Upkeep), (fun x -> Upkeep x)
  | Direct.Victory -> (module Victory), (fun x -> Victory x)
*)
  | _ -> failwith "todo"

let of_cond : Steps.Output.cond -> 'a cond =
  let module Cond = Steps.Output in
  function
(*
  | Cond.Barraged -> (module Barraged), (fun x -> Barraged x)
  | Cond.Defeat -> (module Defeat), (fun x -> Defeat x)
  | Cond.Disease -> (module Disease), (fun x -> Disease x)
  | Cond.HitRun -> (module HitRun), (fun x -> HitRun x)
  | Cond.LeaderLvup -> (module LeaderLvup), (fun x -> LeaderLvup x)
  | Cond.Mangonel -> (module Mangonel), (fun x -> Mangonel x)
  | Cond.Smite -> (module Smite), (fun x -> Smite x)
*)
  | _ -> failwith "todo"
