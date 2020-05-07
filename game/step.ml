type kind = Input of Input.kind | Output of Output.kind
type t =
  { kind : kind
  ; rest : Steps.t list
  ; state : State.t
  }

let input state = function
  | Steps.Cond step -> Input.of_cond step state
  | Steps.Direct step -> Some (Input.of_direct step state)

let output state = function
  | Steps.Cond step -> Output.of_cond step state
  | Steps.Direct step -> Some (Output.of_direct step state)

let rec seek label = function
  | [] -> []
  | Steps.Mark x :: rest ->
      if x = label then rest
      else seek label rest
  | _ :: rest -> seek label rest

let rec next_of state = function
  | [] -> None
  | Steps.Ask x :: rest ->
      begin match input state x with
      | Some evt ->
          Some { kind = Input evt; rest; state }
      | None -> next_of state rest
      end
  | Steps.Do x :: rest ->
      begin match output state x with
      | Some (evt, state) ->
          Some { kind = Output evt; rest; state }
      | None -> next_of state rest
      end
  | Steps.End x :: rest ->
      begin match Output.of_cond x state with
      | Some (evt, state) ->
          Some { kind = Output evt; rest = []; state }
      | None -> next_of state rest
      end
  | Steps.GoTo label :: _ ->
      next_of state (seek label Steps.ls)
  | Steps.Mark _ :: rest ->
      next_of state rest

let next t = next_of t.state t.rest
let first state = next_of state Steps.ls

let kind t = t.kind
let state t = t.state
let state_set state t = { t with state }
