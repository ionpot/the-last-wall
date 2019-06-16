module Steps = Steps.Phase2

module Input = struct
  module Event = Input

  type event =
    | Ballista of Event.Ballista.t
    | Build of Event.BuildAvlb.t
    | Dervish of Event.Dervish.t
    | Mercs of Event.Mercs.t
    | Nations of Event.Nations.t
    | Ranger of Event.Ranger.t
    | Templar of Event.Templar.t
    | Trade of Event.Trade.t

  module Apply (State : State.S) = struct
    module Apply = Phase.Apply(State)
    let event = function
      | Ballista x -> Apply.value x (module Event.Ballista)
      | Build x -> Apply.value x (module Event.BuildAvlb)
      | Dervish x -> Apply.value x (module Event.Dervish)
      | Mercs x -> Apply.value x (module Event.Mercs)
      | Nations x -> Apply.value x (module Event.Nations)
      | Ranger x -> Apply.value x (module Event.Ranger)
      | Templar x -> Apply.value x (module Event.Templar)
      | Trade x -> Apply.value x (module Event.Trade)
  end
end

module Output = struct
  type event =
    | Attack of Direct.Attack.t
    | Blessing of Direct.Blessing.t
    | BuildManp of Direct.BuildManp.t
    | BuildStatus of Direct.BuildStatus.t
    | BuildSupply of Direct.BuildSupply.t
    | Cavalry of Cond.Cavalry.t
    | Defeat
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
      | Steps.Ballista -> (module struct module Event = Event.Ballista
          let make x = Input.Ballista x end)
      | Steps.Build -> (module struct module Event = Event.BuildAvlb
          let make x = Input.Build x end)
      | Steps.Dervish -> (module struct module Event = Event.Dervish
          let make x = Input.Dervish x end)
      | Steps.Nations -> (module struct module Event = Event.Nations
          let make x = Input.Nations x end)

    let cond : Convert.cond = function
      | Steps.Mercs -> (module struct module Event = Event.Mercs
          let make x = Input.Mercs x end)
      | Steps.Ranger -> (module struct module Event = Event.Ranger
          let make x = Input.Ranger x end)
      | Steps.Templar -> (module struct module Event = Event.Templar
          let make x = Input.Templar x end)
      | Steps.Trade -> (module struct module Event = Event.Trade
          let make x = Input.Trade x end)
  end

  module Output = struct
    module Steps = Steps.Output
    module Convert = Phase.Convert.Output(Steps)(Output)

    let check () = failwith "no phase2 check"

    let cond : Convert.cond = function
      | Steps.Cavalry -> (module struct module Event = Cond.Cavalry
          let make x = Output.Cavalry x end)
      | Steps.Defeat -> (module struct module Event = Cond.Defeat
          let make () = Output.Defeat end)
      | Steps.LeaderNew -> (module struct module Event = Cond.LeaderNew
          let make x = Output.LeaderNew x end)
      | Steps.Market -> (module struct module Event = Cond.Market
          let make x = Output.Market x end)
      | Steps.Starvation -> (module struct module Event = Cond.Starvation
          let make x = Output.Starvation x end)

    let direct : Convert.direct = function
      | Steps.Attack -> (module struct module Event = Direct.Attack
          let make x = Output.Attack x end)
      | Steps.Blessing -> (module struct module Event = Direct.Blessing
          let make x = Output.Blessing x end)
      | Steps.BuildManp -> (module struct module Event = Direct.BuildManp
          let make x = Output.BuildManp x end)
      | Steps.BuildStatus -> (module struct module Event = Direct.BuildStatus
          let make x = Output.BuildStatus x end)
      | Steps.BuildSupply -> (module struct module Event = Direct.BuildSupply
          let make x = Output.BuildSupply x end)
      | Steps.Support -> (module struct module Event = Direct.Support
          let make x = Output.Support x end)
      | Steps.Turn -> (module struct module Event = Direct.Turn
          let make x = Output.Turn x end)
      | Steps.Upkeep -> (module struct module Event = Direct.Upkeep
          let make x = Output.Upkeep x end)
  end
end
