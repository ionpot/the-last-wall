module Make (Phase : Phase.S) (State : State.S) = struct
  let check_event (module Event : Event.CanCheck) =
    let module Result = Event.Check(State) in
    Result.value

  module Input = struct
    let check (module M : Phase.Input.Cond) =
      check_event (module M.Event)

    let cond (module M : Phase.Input.Cond) =
      let module Made = M.Event.Make(State) in
      M.make Made.value

    let direct (module M : Phase.Input.Direct) =
      let module Made = M.Event.Make(State) in
      M.make Made.value
  end

  module Output = struct
    let cond_ok (module M : Phase.Output.Cond) =
      check_event (module M.Event)

    let notify_ok (module M : Phase.Output.Notify) =
      check_event (module M.Event)

    let cond (module M : Phase.Output.Cond) =
      let module Made = M.Event.Make(State) in
      let module Apply = M.Event.Apply(State) in
      let apply () = Apply.value Made.value in
      M.make Made.value, apply

    let direct (module M : Phase.Output.Direct) =
      let module Made = M.Event.Make(State) in
      let module Apply = M.Event.Apply(State) in
      let apply () = Apply.value Made.value in
      M.make Made.value, apply

    let notify (module M : Phase.Output.Notify) =
      let module Made = M.Event.Make(State) in
      let apply () = () in
      M.make Made.value, apply
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

  module Do (State : State.S) = struct
    module Make = Make(Phase)(State)
    type found =
      | FoundInput of (Phase.Steps.Input.t * steps)
      | FoundOutput of (Phase.Steps.Output.t * steps)
      | End

    let apply = function
      | Input x -> let module A = Input.Apply(State) in A.event x
      | Output (_, apply) -> apply ()

    let input_of : Phase.Steps.Input.t -> Input.event = function
      | Steps.Cond x -> Make.Input.cond (Input.cond x)
      | Steps.Direct x -> Make.Input.direct (Input.direct x)

    let output_of : Phase.Steps.Output.t -> output = function
      | Steps.Cond x -> Make.Output.cond (Output.cond x)
      | Steps.Direct x -> Make.Output.direct (Output.direct x)
      | Steps.Notify x -> Make.Output.notify (Output.notify x)

    let input_ok : Phase.Steps.Input.t -> bool = function
      | Steps.Cond x -> Make.Input.check (Input.cond x)
      | Steps.Direct _ -> true

    let output_ok : Phase.Steps.Output.t -> bool = function
      | Steps.Cond x -> Make.Output.cond_ok (Output.cond x)
      | Steps.Direct _ -> true
      | Steps.Notify x -> Make.Output.notify_ok (Output.notify x)

    let rec seek = function
      | [] -> End
      | Steps.Ask a :: rest ->
          if input_ok a then FoundInput (a, rest)
          else seek rest
      | Steps.Branch (cond, ls) :: rest ->
          if output_ok cond then FoundOutput (cond, ls)
          else seek rest
      | Steps.Do a :: rest ->
          if output_ok a then FoundOutput (a, rest)
          else seek rest
      | Steps.Either (a, b) :: rest ->
          if output_ok a then FoundOutput (a, rest)
          else seek ((Steps.Do b) :: rest)

    let next steps =
      match seek steps with
      | FoundInput (step, rest) -> Some (Input (input_of step), rest)
      | FoundOutput (step, rest) -> Some (Output (output_of step), rest)
      | End -> None

    let first () =
      next Phase.Steps.list
  end
end
