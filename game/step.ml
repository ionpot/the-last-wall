module Input = struct
  type t = Input.event
  let direct state (evt, fn) =
    Some (Input.make evt state |> fn)
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
  type t = Output.event * State.t
  let direct state (evt, fn) =
    let x, state' = Output.make evt state in
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

type kind = Input of Input.t | Output of Output.t
type ls = Steps.t list
type t = kind * ls

let start = Steps.ls

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
      | Some evt -> Some (Input evt, rest)
      | None -> next state rest
      end
  | Steps.Do x :: rest ->
      begin match Output.from state x with
      | Some evt ->
          let last = x = Steps.Output.last in
          Some (Output evt, if last then [] else rest)
      | None -> next state rest
      end
  | Steps.GoTo label :: _ ->
      next state (seek label start)
  | Steps.Mark _ :: rest ->
      next state rest
