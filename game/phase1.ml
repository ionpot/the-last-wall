module Steps = Steps.Phase1

module Input = struct
  module Steps = Steps.Input
  type event =
    | Build of Input.Build.t
    | Deity of Input.Deity.t
    | Leader of Input.Leader.t
    | Nations of Input.Nations.t
    | Scout of Input.Scout.t
  module type Cond = Phase.Cond with type t := event
  module type Direct = Phase.Direct with type t := event
  let direct : Steps.direct -> (module Direct) = function
    | Steps.Build ->
        (module struct module Event = Input.Build let make x = Build x end)
    | Steps.Deity ->
        (module struct module Event = Input.Deity let make x = Deity x end)
    | Steps.Leader ->
        (module struct module Event = Input.Leader let make x = Leader x end)
    | Steps.Nations ->
        (module struct module Event = Input.Nations let make x = Nations x end)
    | Steps.Scout ->
        (module struct module Event = Input.Scout let make x = Scout x end)
  let cond () = failwith "no phase1 input cond"
  module Apply (State : State.S) = struct
    module Apply = Phase.Apply(State)
    let event = function
      | Build x -> Apply.value x (module Input.Build)
      | Deity x -> Apply.value x (module Input.Deity)
      | Leader x -> Apply.value x (module Input.Leader)
      | Nations x -> Apply.value x (module Input.Nations)
      | Scout x -> Apply.value x (module Input.Scout)
  end
end

module Output = struct
  module Steps = Steps.Output
  type event =
    | BuildSupply of Direct.BuildSupply.t
    | Starting of Direct.Starting.t
    | Support of Direct.Support.t
  let cond () = failwith "no phase1 output cond"
  module type Cond = Phase.Cond with type t := event
  module type Direct = Phase.Direct with type t := event
  module type Notify = Phase.Notify with type t := event
  let direct : Steps.direct -> (module Direct) = function
    | Steps.BuildSupply ->
        (module struct module Event = Direct.BuildSupply
          let make x = BuildSupply x end)
    | Steps.Starting ->
        (module struct module Event = Direct.Starting
          let make x = Starting x end)
    | Steps.Support ->
        (module struct module Event = Direct.Support
          let make x = Support x end)
  let notify () = failwith "no phase1 output notify"
end
