module Steps = Steps.Phase1

module Input = struct
  module Event = Input
  type event =
    | Ballista of Event.Ballista.t
    | Build of Event.BuildAvlb.t
    | Deity of Event.DeityChoice.t
    | Knight of Event.Knight.t
    | Leader of Event.LeaderKind.t
    | Nations of Event.Nations.t
    | Scout of Event.Scout.t
    | Trade of Event.Trade.t
    | Volunteers of Event.Volunteers.t
  module Apply (State : State.S) = struct
    module Apply = Phase.Apply(State)
    let event = function
      | Ballista x -> Apply.value x (module Event.Ballista)
      | Build x -> Apply.value x (module Event.BuildAvlb)
      | Deity x -> Apply.value x (module Event.DeityChoice)
      | Knight x -> Apply.value x (module Event.Knight)
      | Leader x -> Apply.value x (module Event.LeaderKind)
      | Nations x -> Apply.value x (module Event.Nations)
      | Scout x -> Apply.value x (module Event.Scout)
      | Trade x -> Apply.value x (module Event.Trade)
      | Volunteers x -> Apply.value x (module Event.Volunteers)
  end
end

module Output = struct
  type event =
    | BuildSupply of Direct.BuildSupply.t
    | Cavalry of Direct.Cavalry.t
    | Facilities of Direct.Facilities.t
    | Starting of Direct.Starting.t
    | Support of Direct.Support.t
end

module Convert = struct
  module Input = struct
    module Event = Input.Event
    module Steps = Steps.Input
    module Convert = Phase.Convert.Input(Steps)(Input)

    let cond : Convert.cond = function
      | Steps.Trade -> (module struct module Event = Event.Trade
          let make x = Input.Trade x end)
      | Steps.Volunteers -> (module struct module Event = Event.Volunteers
          let make x = Input.Volunteers x end)

    let direct : Convert.direct = function
      | Steps.Ballista -> (module struct module Event = Event.Ballista
          let make x = Input.Ballista x end)
      | Steps.Build -> (module struct module Event = Event.BuildAvlb
          let make x = Input.Build x end)
      | Steps.Deity -> (module struct module Event = Event.DeityChoice
          let make x = Input.Deity x end)
      | Steps.Knight -> (module struct module Event = Event.Knight
          let make x = Input.Knight x end)
      | Steps.Leader -> (module struct module Event = Event.LeaderKind
          let make x = Input.Leader x end)
      | Steps.Nations -> (module struct module Event = Event.Nations
          let make x = Input.Nations x end)
      | Steps.Scout -> (module struct module Event = Event.Scout
          let make x = Input.Scout x end)
  end

  module Output = struct
    module Steps = Steps.Output
    module Convert = Phase.Convert.Output(Steps)(Output)

    let check () = failwith "no phase1 output check"
    let cond () = failwith "no phase1 output cond"

    let direct : Convert.direct = function
      | Steps.BuildSupply -> (module struct module Event = Direct.BuildSupply
          let make x = Output.BuildSupply x end)
      | Steps.Cavalry -> (module struct module Event = Direct.Cavalry
          let make x = Output.Cavalry x end)
      | Steps.Facilities -> (module struct module Event = Direct.Facilities
          let make x = Output.Facilities x end)
      | Steps.Starting -> (module struct module Event = Direct.Starting
          let make x = Output.Starting x end)
      | Steps.Support -> (module struct module Event = Direct.Support
          let make x = Output.Support x end)
  end
end
