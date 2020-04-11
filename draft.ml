type state = { x : int }

module Event = struct
  module type Direct = sig
    type t
    val apply : state -> t -> state
    val make : state -> t
  end
  module type Cond = sig
    include Direct
    val check : state -> bool
  end
  type 'a direct = (module Direct with type t = 'a)
  type 'a cond = (module Cond with type t = 'a)
  module Input : sig
    type 'a t
    val make : 'a direct -> state -> 'a t
    val apply : 'a t -> (state -> 'a -> state)
    val value : 'a t -> 'a
  end = struct
    type 'a t =
      { apply : state -> 'a -> state
      ; value : 'a
      }
    let make (type a) (module M : Direct with type t = a) state =
      { apply = M.apply
      ; value = M.make state
      }
    let apply t = t.apply
    let value t = t.value
  end
end

module Steps = struct
  type ('d, 'c) event = Direct of 'd | Cond of 'c
  module Input = struct
    type cond = A
    type t = (unit, cond) event
  end
  module Output = struct
    type direct = A
    type cond = B
    type t = (direct, cond) event
    let last = Cond B
  end
  type t = Ask of Input.t | Do of Output.t
  let start : t list =
    [ Ask (Cond Input.A)
    ; Do (Cond Output.B)
    ; Do (Direct Output.A)
    ]
end

module Input = struct
  module A = struct
    type t = int
    let apply s t = { x = s.x + t }
    let check s = s.x > 0
    let make s = s.x
  end
  type 'a t = 'a Event.Input.t
  type event = A of A.t t
  type 'a convert = 'a t -> event
  type 'a cond = 'a Event.cond * 'a convert
  let of_cond : Steps.Input.cond -> 'a cond =
    let module Cond = Steps.Input in
    function
    | Cond.A -> (module A), (fun x -> A x)
end

module Output = struct
  module A = struct
    type t = int
    let apply s _ = s
    let make s = s.x
  end
  module B = struct
    type t = unit
    let apply s _ = s
    let check s = true
    let make s = ()
  end
  type event = A of A.t | B of B.t
  type 'a convert = 'a -> event
  type 'a direct = 'a Event.direct * 'a convert
  type 'a cond = 'a Event.cond * 'a convert
  type 'a t = 'a * state
  let make (type a) (module M : Event.Direct with type t = a) state =
    let x = M.make state in
    x, M.apply state x
  let of_cond : Steps.Output.cond -> 'a cond =
    let module Cond = Steps.Output in
    function
    | Cond.B -> (module B), (fun x -> B x)
  let of_direct : Steps.Output.direct -> 'a direct =
    let module Direct = Steps.Output in
    function
    | Direct.A -> (module A), (fun x -> A x)
end

module Step = struct
  module Input = struct
    type t = Input.event * state
    let direct state (evt, fn) =
      Some (Event.Input.make evt state |> fn, state)
    let cond : type a. state -> a Input.cond -> t option =
      fun state ((module M), fn) ->
        if M.check state
        then direct state ((module M), fn)
        else None
    let from state = function
      | Steps.Cond step -> Input.of_cond step |> cond state
      | Steps.Direct () -> None
  end
  module Output = struct
    type t = Output.event * state
    let direct state (evt, fn) =
      let x, state' = Output.make evt state in
      Some (fn x, state')
    let cond : type a. state -> a Output.cond -> t option =
      fun state ((module M), fn) ->
        if M.check state
        then direct state ((module M), fn)
        else None
    let from state = function
      | Steps.Cond step -> Output.of_cond step |> cond state
      | Steps.Direct step -> Output.of_direct step |> direct state
  end
  type event = Input of Input.t | Output of Output.t
  type t = event * Steps.t list
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
end

module Main = struct
  type step =
    | Next of Step.t
    | End
  let make = function
    | Some step -> Next step
    | None -> End
  let next steps state =
    Step.next state steps |> make
  let first = next Steps.start
end

let handle_input = function
  | Input.A e, state ->
      Printf.printf "input A = %d\n" (Event.Input.value e);
      read_int () |> Event.Input.apply e state

let print_output = function
  | Output.A x ->
      Printf.printf "output A = %d\n" x
  | Output.B () ->
      print_endline "output B"

let handle = function
  | Step.Input evt ->
      handle_input evt
  | Step.Output (evt, state') ->
      print_output evt; state'

let rec check = function
  | Main.Next (step, steps) ->
      handle step
      |> Main.next steps
      |> check
  | Main.End -> ()

let _ =
  let state = { x = 1 } in
  Main.first state
  |> check
