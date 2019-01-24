type 'a t =
  | This of 'a
  | Next
  | End

let rec is_in evt = function
  | Defs.Do x -> x = evt
  | Defs.Either (a, b) -> is_in evt a || is_in evt b
  | Defs.Last -> false

let slice event steps =
  Listx.slice_from (is_in event) steps

module Seek (P : Phase.S) (State : State.S) = struct
  type _ t =
    | Cond : P.cond t
    | Input : P.input t
    | Notify : P.notify t

  let to_check x = function
    | Cond -> P.Check.cond x
    | Input -> P.Check.input x
    | Notify -> P.Check.notify x

  let to_bool x t =
    let module Check = (val to_check x t) in
    let module M = Check(State) in
    M.value

  let is_ok = function
    | Defs.Cond x -> to_bool x Cond
    | Defs.Direct _ -> true
    | Defs.Input x -> to_bool x Input
    | Defs.Nothing -> false
    | Defs.Notify x -> to_bool x Notify

  let rec drill = function
    | Defs.Do x -> if is_ok x then Some x else None
    | Defs.Either (a, b) ->
        Listx.first_some [
          (fun () -> drill a);
          (fun () -> drill b)]
    | Defs.Last -> None

  let rec find = function
    | [] -> None
    | Defs.Last :: rest -> find (Listx.last_of rest)
    | step :: rest ->
        Listx.first_some [
          (fun () -> drill step);
          (fun () -> find rest)]

  let first () =
    find P.steps

  let next_of event =
    find (slice event P.steps)
end

module Apply (P : Phase.S) (State : State.S) = struct
  module M = P.Apply(State)
  let value = function
    | Phase.Event x -> M.event x
    | Phase.Input x -> M.input x
    | Phase.Nothing
    | Phase.Notify _ -> ()
end

module First (P : Phase.S) (State : State.S) = struct
  module Seek = Seek(P)(State)
  let value = Seek.first ()
end

module Next (P : Phase.S) (State : State.S) = struct
  module Seek = Seek(P)(State)

  let event_of = function
    | Phase.Event x -> P.Event.of_event x
    | Phase.Input x -> P.Event.of_input x
    | Phase.Nothing -> Defs.Nothing
    | Phase.Notify x -> P.Event.of_notify x

  let output_of event =
    let module M = P.Make(State) in
    match event with
    | Defs.Cond x -> M.cond x
    | Defs.Direct x -> M.direct x
    | Defs.Input x -> M.input x
    | Defs.Nothing -> Phase.Nothing
    | Defs.Notify x -> M.notify x

  let next_of output =
    Seek.next_of (event_of output)

  let value output =
    if P.is_end output then End
    else match next_of output with
    | Some event -> This (output_of event)
    | None -> Next
end
