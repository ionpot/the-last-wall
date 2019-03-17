let is_in event = function
  | Steps.Do a
  | Steps.JumpIfNo (a, _) -> a = event
  | Steps.Either (a, b) -> a = event || b = event

let slice_from event steps =
  Listx.slice_from (is_in event) steps

module Of (Phase : Phase.S) = struct
  module Output = Phase.Output

  type output =
    | Event of Output.event
    | Input of Output.input
    | Notify of Output.notify
  type steps = Phase.Steps.t
  type event = output * steps

  let is_end = function
    | Event _
    | Input _ -> false
    | Notify n -> Output.is_end n

  module Apply (State : State.S) = struct
    module Apply = Output.Apply(State)

    let value = function
      | Event e -> Apply.event e
      | Input i -> Apply.input i
      | Notify _ -> ()
  end

  module Seek (State : State.S) = struct
    module Make = Phase.Make(State)

    let output_of = function
      | Steps.Cond x -> Event (Make.cond x)
      | Steps.Direct x -> Event (Make.direct x)
      | Steps.Input x -> Input (Make.input x)
      | Steps.Notify x -> Notify (Make.notify x)

    let to_bool (module Check : Event.Check) =
      let module Result = Check(State) in
      Result.value

    let is_ok = function
      | Steps.Cond x -> to_bool (Phase.Check.cond x)
      | Steps.Direct _ -> true
      | Steps.Input x -> to_bool (Phase.Check.input x)
      | Steps.Notify x -> to_bool (Phase.Check.notify x)

    let rec seek = function
      | [] -> None
      | Steps.Do a :: rest ->
          if is_ok a then Some (a, rest)
          else seek rest
      | Steps.Either (a, b) :: rest ->
          if is_ok a then Some (a, rest)
          else seek ((Steps.Do b) :: rest)
      | Steps.JumpIfNo (a, b) :: rest ->
          if is_ok a then Some (a, rest)
          else seek (slice_from b rest)

    let next steps =
      match seek steps with
      | Some (etype, rest) -> Some (output_of etype, rest)
      | None -> None

    let first () =
      next Phase.Steps.list
  end
end
