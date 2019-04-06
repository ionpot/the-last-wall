module Make (P : Phase.S) (State : State.S) = struct
  let check_event (module Event : Event.CanCheck) =
    let module Result = Event.Check(State) in
    Result.value

  module Input = struct
    module Convert = Phase.Convert.Input(P.Steps.Input)(P.Input)

    let check (module M : Convert.Cond) =
      check_event (module M.Event)

    let cond (module M : Convert.Cond) =
      let module Made = M.Event.Make(State) in
      M.make Made.value

    let direct (module M : Convert.Direct) =
      let module Made = M.Event.Make(State) in
      M.make Made.value
  end

  module Output = struct
    module Convert = Phase.Convert.Output(P.Steps.Output)(P.Output)

    let cond_ok (module M : Convert.Cond) =
      check_event (module M.Event)

    let notify_ok (module M : Convert.Notify) =
      check_event (module M.Event)

    let cond (module M : Convert.Cond) =
      let module Made = M.Event.Make(State) in
      let module Apply = M.Event.Apply(State) in
      let apply () = Apply.value Made.value in
      M.make Made.value, apply

    let direct (module M : Convert.Direct) =
      let module Made = M.Event.Make(State) in
      let module Apply = M.Event.Apply(State) in
      let apply () = Apply.value Made.value in
      M.make Made.value, apply

    let notify (module M : Convert.Notify) =
      let module Made = M.Event.Make(State) in
      let apply () = () in
      M.make Made.value, apply
  end
end

module Convert (Phase : Phase.S) (State : State.S) = struct
  module Make = Make(Phase)(State)
  module Input = struct
    module Convert = Phase.Convert.Input
    module Make = Make.Input
    let check step = Make.check (Convert.cond step)
    let cond step = Make.cond (Convert.cond step)
    let direct step = Make.direct (Convert.direct step)
  end
  module Output = struct
    module Convert = Phase.Convert.Output
    module Make = Make.Output
    let check_cond step = Make.cond_ok (Convert.cond step)
    let check_notify step = Make.notify_ok (Convert.notify step)
    let cond step = Make.cond (Convert.cond step)
    let direct step = Make.direct (Convert.direct step)
    let notify step = Make.notify (Convert.notify step)
  end
end

module Of (Phase : Phase.S) = struct
  module Input = Phase.Input
  module Output = Phase.Output

  type apply = unit -> unit
  type output = (Output.event * apply)
  type event =
    | Input of Input.event
    | Output of output
  type steps = Phase.Steps.t list
  type t = event * steps

  let first = Phase.Steps.list

  module Do (State : State.S) = struct
    module Convert = Convert(Phase)(State)

    type found =
      | FoundInput of (Phase.Steps.Input.t * steps)
      | FoundOutput of (Phase.Steps.Output.t * steps)
      | End

    let apply = function
      | Input x -> let module A = Input.Apply(State) in A.event x
      | Output (_, apply) -> apply ()

    let input_of : Phase.Steps.Input.t -> Input.event = function
      | Steps.Cond step -> Convert.Input.cond step
      | Steps.Direct step -> Convert.Input.direct step

    let output_of : Phase.Steps.Output.t -> output = function
      | Steps.Cond step -> Convert.Output.cond step
      | Steps.Direct step -> Convert.Output.direct step
      | Steps.Notify step -> Convert.Output.notify step

    let input_ok : Phase.Steps.Input.t -> bool = function
      | Steps.Cond step -> Convert.Input.check step
      | Steps.Direct _ -> true

    let output_ok : Phase.Steps.Output.t -> bool = function
      | Steps.Cond step -> Convert.Output.check_cond step
      | Steps.Direct _ -> true
      | Steps.Notify step -> Convert.Output.check_notify step

    let rec seek = function
      | [] -> End
      | Steps.Ask a :: rest ->
          if input_ok a then FoundInput (a, rest)
          else seek rest
      | Steps.Do a :: rest ->
          if output_ok a then FoundOutput (a, rest)
          else seek rest
      | Steps.Go ls :: _ -> seek ls
      | Steps.GoIf (cond, ls) :: rest ->
          if output_ok cond then FoundOutput (cond, ls)
          else seek rest

    let next steps =
      match seek steps with
      | FoundInput (step, rest) -> Some (Input (input_of step), rest)
      | FoundOutput (step, rest) -> Some (Output (output_of step), rest)
      | End -> None
  end
end
