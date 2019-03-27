module Output = struct
  type event =
    | Blessing of Direct.Blessing.t
    | BuildManp of Direct.BuildManp.t
    | BuildStatus of Direct.BuildStatus.t
    | BuildSupply of Direct.BuildSupply.t
    | Cavalry of Cond.Cavalry.t
    | Defeat of Cond.Defeat.t
    | Enemies of Direct.Enemies.t
    | LeaderNew of Cond.LeaderNew.t
    | Market of Cond.Market.t
    | Starvation of Cond.Starvation.t
    | Support of Direct.Support.t
    | Turn of Direct.Turn.t
    | Upkeep of Direct.Upkeep.t

  type input =
    | Build of Input.Build.t
    | Mercs of Input.Mercs.t
    | Nations of Input.Nations.t

  type notify = unit

  module Apply (State : State.S) = struct
    module Apply = Event.Apply(State)

    let event = function
      | Blessing x -> Apply.value x (module Direct.Blessing)
      | BuildManp x -> Apply.value x (module Direct.BuildManp)
      | BuildStatus x -> Apply.value x (module Direct.BuildStatus)
      | BuildSupply x -> Apply.value x (module Direct.BuildSupply)
      | Cavalry x -> Apply.value x (module Cond.Cavalry)
      | Defeat x -> Apply.value x (module Cond.Defeat)
      | Enemies x -> Apply.value x (module Direct.Enemies)
      | LeaderNew x -> Apply.value x (module Cond.LeaderNew)
      | Market x -> Apply.value x (module Cond.Market)
      | Starvation x -> Apply.value x (module Cond.Starvation)
      | Support x -> Apply.value x (module Direct.Support)
      | Turn x -> Apply.value x (module Direct.Turn)
      | Upkeep x -> Apply.value x (module Direct.Upkeep)

    let input = function
      | Build x -> Apply.value x (module Input.Build)
      | Mercs x -> Apply.value x (module Input.Mercs)
      | Nations x -> Apply.value x (module Input.Nations)
  end
end

module S = struct
  module Output = Output
  module Steps = Steps.Phase2

  open Output

  module Make = struct
    let cond = function
      | Steps.Cavalry ->
          Phase.event (module Cond.Cavalry) (fun x -> Cavalry x)
      | Steps.Defeat ->
          Phase.event (module Cond.Defeat) (fun x -> Defeat x)
      | Steps.LeaderNew ->
          Phase.event (module Cond.LeaderNew) (fun x -> LeaderNew x)
      | Steps.Market ->
          Phase.event (module Cond.Market) (fun x -> Market x)
      | Steps.Starvation ->
          Phase.event (module Cond.Starvation) (fun x -> Starvation x)

    let direct = function
      | Steps.Blessing ->
          Phase.event (module Cond.Blessing) (fun x -> Blessing x)
      | Steps.BuildManp ->
          Phase.event (module Cond.BuildManp) (fun x -> BuildManp x)
      | Steps.BuildStatus ->
          Phase.event (module Cond.BuildStatus) (fun x -> BuildStatus x)
      | Steps.BuildSupply ->
          Phase.event (module Cond.BuildSupply) (fun x -> BuildSupply x)
      | Steps.Enemies ->
          Phase.event (module Cond.Enemies) (fun x -> Enemies x)
      | Steps.Support ->
          Phase.event (module Cond.Support) (fun x -> Support x)
      | Steps.Turn ->
          Phase.event (module Cond.Turn) (fun x -> Turn x)
      | Steps.Upkeep ->
          Phase.event (module Cond.Upkeep) (fun x -> Upkeep x)

    let input = function
      | Steps.Build ->
          Phase.input (module Input.Build) (fun x -> Build x)
      | Steps.Mercs ->
          Phase.input (module Input.Mercs) (fun x -> Mercs x)
      | Steps.Nations ->
          Phase.input (module Input.Nations) (fun x -> Nations x)

    let notify = Phase.no_notify
  end
end
