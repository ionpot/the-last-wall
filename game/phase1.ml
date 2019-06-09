module Steps = Steps.Phase1

module Input = struct
  module Event = Input
  type event =
    | Build of Event.BuildAvlb.t
    | Deity of Event.DeityChoice.t
    | Leader of Event.Leader.t
    | Nations of Event.Nations.t
    | Scout of Event.Scout.t
  module Apply (State : State.S) = struct
    module Apply = Phase.Apply(State)
    let event = function
      | Build x -> Apply.value x (module Event.BuildAvlb)
      | Deity x -> Apply.value x (module Event.DeityChoice)
      | Leader x -> Apply.value x (module Event.Leader)
      | Nations x -> Apply.value x (module Event.Nations)
      | Scout x -> Apply.value x (module Event.Scout)
  end
end

module Output = struct
  type event =
    | BuildSupply of Direct.BuildSupply.t
    | Starting of Direct.Starting.t
    | Support of Direct.Support.t
end

module Convert = struct
  module Input = struct
    module Event = Input.Event
    module Steps = Steps.Input
    module Convert = Phase.Convert.Input(Steps)(Input)

    let cond () = failwith "no phase1 input cond"

    let direct : Convert.direct = function
      | Steps.Build -> (module struct module Event = Event.BuildAvlb
          let make x = Input.Build x end)
      | Steps.Deity -> (module struct module Event = Event.DeityChoice
          let make x = Input.Deity x end)
      | Steps.Leader -> (module struct module Event = Event.Leader
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
      | Steps.Starting -> (module struct module Event = Direct.Starting
          let make x = Output.Starting x end)
      | Steps.Support -> (module struct module Event = Direct.Support
          let make x = Output.Support x end)
  end
end
