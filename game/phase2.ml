module Steps = Steps.Phase2

module Input = struct
  module Steps = Steps.Input

  type event =
    | Build of Input.Build.t
    | Mercs of Input.Mercs.t
    | Nations of Input.Nations.t

  module type Cond = Phase.Cond with type t := event
  module type Direct = Phase.Direct with type t := event

  let direct : Steps.direct -> (module Direct) = function
    | Steps.Build ->
        (module struct module Event = Input.Build let make x = Build x end)
    | Steps.Nations ->
        (module struct module Event = Input.Nations let make x = Nations x end)

  let cond : Steps.cond -> (module Cond) = function
    | Steps.Mercs ->
        (module struct module Event = Input.Mercs let make x = Mercs x end)

  module Apply (State : State.S) = struct
    module Apply = Phase.Apply(State)
    let event = function
      | Build x -> Apply.value x (module Input.Build)
      | Mercs x -> Apply.value x (module Input.Mercs)
      | Nations x -> Apply.value x (module Input.Nations)
  end
end

module Output = struct
  module Steps = Steps.Output

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

  module type Cond = Phase.Cond with type t := event
  module type Direct = Phase.Direct with type t := event
  module type Notify = Phase.Notify with type t := event

  let cond : Steps.cond -> (module Cond) = function
    | Steps.Cavalry ->
        (module struct module Event = Cond.Cavalry
          let make x = Cavalry x end)
    | Steps.Defeat ->
        (module struct module Event = Cond.Defeat
          let make x = Defeat x end)
    | Steps.LeaderNew ->
        (module struct module Event = Cond.LeaderNew
          let make x = LeaderNew x end)
    | Steps.Market ->
        (module struct module Event = Cond.Market
          let make x = Market x end)
    | Steps.Starvation ->
        (module struct module Event = Cond.Starvation
          let make x = Starvation x end)

  let direct : Steps.direct -> (module Direct) = function
    | Steps.Blessing ->
        (module struct module Event = Direct.Blessing
          let make x = Blessing x end)
    | Steps.BuildManp ->
        (module struct module Event = Direct.BuildManp
          let make x = BuildManp x end)
    | Steps.BuildStatus ->
        (module struct module Event = Direct.BuildStatus
          let make x = BuildStatus x end)
    | Steps.BuildSupply ->
        (module struct module Event = Direct.BuildSupply
          let make x = BuildSupply x end)
    | Steps.Enemies ->
        (module struct module Event = Direct.Enemies
          let make x = Enemies x end)
    | Steps.Support ->
        (module struct module Event = Direct.Support
          let make x = Support x end)
    | Steps.Turn ->
        (module struct module Event = Direct.Turn
          let make x = Turn x end)
    | Steps.Upkeep ->
        (module struct module Event = Direct.Upkeep
          let make x = Upkeep x end)

  let notify () = failwith "no phase2 notify"
end
