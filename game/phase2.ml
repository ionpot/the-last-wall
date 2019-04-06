module Steps = Steps.Phase2

module Input = struct
  module Event = Input

  type event =
    | Build of Event.Build.t
    | Mercs of Event.Mercs.t
    | Nations of Event.Nations.t

  module Apply (State : State.S) = struct
    module Apply = Phase.Apply(State)
    let event = function
      | Build x -> Apply.value x (module Event.Build)
      | Mercs x -> Apply.value x (module Event.Mercs)
      | Nations x -> Apply.value x (module Event.Nations)
  end
end

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
end

module Convert = struct
  module Input = struct
    module Event = Input.Event
    module Steps = Steps.Input
    module Convert = Phase.Convert.Input(Steps)(Input)

    let direct : Convert.direct = function
      | Steps.Build -> (module struct module Event = Event.Build
          let make x = Input.Build x end)
      | Steps.Nations -> (module struct module Event = Event.Nations
          let make x = Input.Nations x end)

    let cond : Convert.cond = function
      | Steps.Mercs -> (module struct module Event = Event.Mercs
          let make x = Input.Mercs x end)
  end

  module Output = struct
    module Steps = Steps.Output
    module Convert = Phase.Convert.Output(Steps)(Output)

    let cond : Convert.cond = function
      | Steps.Cavalry -> (module struct module Event = Cond.Cavalry
          let make x = Output.Cavalry x end)
      | Steps.Defeat -> (module struct module Event = Cond.Defeat
          let make x = Output.Defeat x end)
      | Steps.LeaderNew -> (module struct module Event = Cond.LeaderNew
          let make x = Output.LeaderNew x end)
      | Steps.Market -> (module struct module Event = Cond.Market
          let make x = Output.Market x end)
      | Steps.Starvation -> (module struct module Event = Cond.Starvation
          let make x = Output.Starvation x end)

    let direct : Convert.direct = function
      | Steps.Blessing -> (module struct module Event = Direct.Blessing
          let make x = Output.Blessing x end)
      | Steps.BuildManp -> (module struct module Event = Direct.BuildManp
          let make x = Output.BuildManp x end)
      | Steps.BuildStatus -> (module struct module Event = Direct.BuildStatus
          let make x = Output.BuildStatus x end)
      | Steps.BuildSupply -> (module struct module Event = Direct.BuildSupply
          let make x = Output.BuildSupply x end)
      | Steps.Enemies -> (module struct module Event = Direct.Enemies
          let make x = Output.Enemies x end)
      | Steps.Support -> (module struct module Event = Direct.Support
          let make x = Output.Support x end)
      | Steps.Turn -> (module struct module Event = Direct.Turn
          let make x = Output.Turn x end)
      | Steps.Upkeep -> (module struct module Event = Direct.Upkeep
          let make x = Output.Upkeep x end)

    let notify () = failwith "no phase2 notify"
  end
end
