module Input = struct
  type t = Input.kind
  let direct state (evt, fn) =
    Some (Event.Input.make evt state |> fn)
  let cond : type a. State.t -> a Input.cond -> t option =
    fun state ((module M), fn) ->
      if M.check state
      then direct state ((module M), fn)
      else None
  let from state = function
    | Steps.Cond step -> Input.of_cond step |> cond state
    | Steps.Direct step -> Input.of_direct step |> direct state
end

module Output = struct
  type t = Output.kind * State.t
  let direct state (evt, fn) =
    let x, state' = Event.Output.make evt state in
    Some (fn x, state')
  let cond : type a. State.t -> a Output.cond -> t option =
    fun state ((module M), fn) ->
      if M.check state
      then direct state ((module M), fn)
      else None
  let from state = function
    | Steps.Cond step -> Output.of_cond step |> cond state
    | Steps.Direct step -> Output.of_direct step |> direct state
end

type kind = Input of Input.kind | Output of Output.kind
type t =
  { kind : kind
  ; rest : Steps.t list
  ; state : State.t
  }

let rec seek label = function
  | [] -> []
  | Steps.Mark x :: rest ->
      if x = label then rest
      else seek label rest
  | _ :: rest -> seek label rest

let rec next state = function
  | [] -> None
  | Steps.Ask x :: rest ->
      begin match Input.from state x with
      | Some evt ->
          Some { kind = Input evt; rest; state }
      | None -> next state rest
      end
  | Steps.Do x :: rest ->
      begin match Output.from state x with
      | Some (evt, state) ->
          let last = x = Steps.Output.last in
          let rest = if last then [] else rest in
          Some { kind = Output evt; rest; state }
      | None -> next state rest
      end
  | Steps.GoTo label :: _ ->
      next state (seek label Steps.ls)
  | Steps.Mark _ :: rest ->
      next state rest

let kind t = t.kind
let rest t = t.rest
let state t = t.state
