module Of (Phase : Phase.S) = struct
  module Output = Phase.Output

  type apply = unit -> unit
  type output =
    | Event of (Output.event * apply)
    | Input of Output.input
    | Notify of Output.notify
  type steps = Phase.Steps.t
  type event = output * steps

  module Do (State : State.S) = struct
    module Make = Phase.Make

    let apply = function
      | Event (_, apply) -> apply ()
      | Input i -> let module A = Output.Apply(State) in A.input i
      | Notify _ -> ()

    let make ((module Event : Event.Direct), convert) =
      let module Make = Event.Make(State) in
      let module Apply = Event.Apply(State) in
      let apply () = Apply.value Make.value in
      Event (convert Make.value, apply)

    let make_input ((module Event : Event.Input), convert) =
      let module Make = Event.Make(State) in
      Input (convert Make.value)

    let make_nfy ((module Event : Event.CanMake), convert) =
      let module Make = Event.Make(State) in
      Notify (convert Make.value)

    let output_of = function
      | Steps.Cond x -> make (Make.cond x)
      | Steps.Direct x -> make (Make.direct x)
      | Steps.Input x -> make_input (Make.input x)
      | Steps.Notify x -> make_nfy (Make.notify x)

    let to_bool ((module Event : Event.CanCheck), _) =
      let module Result = Event.Check(State) in
      Result.value

    let is_ok = function
      | Steps.Cond x -> to_bool (Make.cond x)
      | Steps.Direct _ -> true
      | Steps.Input x -> to_bool (Make.input x)
      | Steps.Notify x -> to_bool (Make.notify x)

    let rec seek = function
      | [] -> None
      | Steps.Do a :: rest ->
          if is_ok a then Some (a, rest)
          else seek rest
      | Steps.Either (a, b) :: rest ->
          if is_ok a then Some (a, rest)
          else seek ((Steps.Do b) :: rest)
      | Steps.Branch (cond, ls) :: rest ->
          if is_ok cond then Some (cond, ls)
          else seek rest

    let next steps =
      match seek steps with
      | Some (etype, rest) -> Some (output_of etype, rest)
      | None -> None

    let first () =
      next Phase.Steps.list
  end
end
