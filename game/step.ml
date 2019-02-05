let is_in event = function
  | Phase.Do a
  | Phase.JumpIfNo (a, _) -> a = event
  | Phase.Either (a, b) -> a = event || b = event

let slice_from event steps =
  Listx.slice_from (is_in event) steps

module Next (Steps : Phase.Steps) (State : State.S) = struct
  type _ t =
    | Cond : Steps.cond t
    | Input : Steps.input t
    | Notify : Steps.notify t

  let to_check : type a. a -> a t -> (module Event.Check) =
    fun x -> function
      | Cond -> Steps.Check.cond x
      | Input -> Steps.Check.input x
      | Notify -> Steps.Check.notify x

  let to_bool x t =
    let module Check = (val to_check x t) in
    let module M = Check(State) in
    M.value

  let is_ok = function
    | Phase.Cond x -> to_bool x Cond
    | Phase.Direct _ -> true
    | Phase.Input x -> to_bool x Input
    | Phase.Notify x -> to_bool x Notify

  let rec next_of = function
    | [] -> None
    | Phase.Do a :: rest ->
        if is_ok a then Some (a, rest)
        else next_of rest
    | Phase.Either (a, b) :: rest ->
        if is_ok a then Some (a, rest)
        else next_of ((Phase.Do b) :: rest)
    | Phase.JumpIfNo (a, b) :: rest ->
        if is_ok a then Some (a, rest)
        else next_of (slice_from b rest)

  let output_of event =
    let module M = Steps.Make(State) in
    match event with
    | Phase.Cond x -> Phase.Event (M.cond x)
    | Phase.Direct x -> Phase.Event (M.direct x)
    | Phase.Input x -> Phase.Input (M.input x)
    | Phase.Notify x -> Phase.Notify (M.notify x)

  let value steps =
    match next_of steps with
    | Some (event, steps) -> Some (output_of event, steps)
    | None -> None
end
