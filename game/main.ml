type event =
  | Ph1 of (Phase1.Output.t * Phase1.Steps.t)
  | Ph2 of (Phase2.Output.t * Phase2.Steps.t)
  | Ph3 of (Phase3.Output.t * Phase3.Steps.t)
  | End

type _ phase =
  | One : Phase1.Steps.t phase
  | Two : Phase2.Steps.t phase
  | Three : Phase3.Steps.t phase

let steps_of : type s. s phase -> (module Phase.Steps with type t = s) = function
  | One -> (module Phase1.Steps)
  | Two -> (module Phase2.Steps)
  | Three -> (module Phase3.Steps)
(*
module Make (State : State.S) = struct
  let rec next_of steps p =
    let module Steps = (val steps_of p) in
    let module N = Step.Next(Steps)(State) in
    match N.value steps, p with
    | Some e, One _ -> Ph1 e
    | Some e, Two _ -> Ph2 e
    | Some e, Three _ -> Ph3 e
    | None, One _ -> first_of (Two (module Phase2.Steps))
    | None, Two _ -> first_of (Three (module Phase3.Steps))
    | None, Three _ -> first_of (Two (module Phase2.Steps))

  and first_of p =
    let module Steps = (val steps_of p) in
    next_of Steps.steps p
end

module First (S : State.S) = struct
  module M = Make(S)
  let value = M.first_of One
end

module Next (S : State.S) = struct
  let apply output phase =
    let module P = (val steps_of phase) in
    let module A = P.Apply(S) in
    match output with
    | Event x -> A.event x
    | Input x -> A.input x
    | Notify _ -> ()

  let is_end output phase =
    let module P = (val steps_of phase) in
    P.is_end output

  let next_of steps phase =
    let module M = Make(S) in
    M.next_of steps phase

  let handle : type o s. (o * s) -> (o, s) phase -> event =
    fun (output, steps) phase ->
      apply output phase;
      if is_end output phase then End
      else next_of steps phase

  let value = function
    | Ph1 x -> handle x One
    | Ph2 x -> handle x Two
    | Ph3 x -> handle x Three
    | End -> End
end

module Handle (State : State.S) (Steps : Phase.Steps) = struct
  let apply output =
    let module A = Steps.Apply(State) in
    match output with
    | Event x -> A.event x
    | Input x -> A.input x
    | Notify _ -> ()

  let next_of steps phase =
    let module S = Seek(Steps)(State) in
    match S.value steps with
    | Some x ->

  let value (output, steps) phase =
    apply output;
    if Steps.is_end output then End
    else next_of steps phase
end*)
