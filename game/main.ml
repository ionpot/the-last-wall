type event =
  | Ph1 of (Phase1.Output.t * Phase1.Steps.t)
  | Ph2 of (Phase2.Output.t * Phase2.Steps.t)
  | Ph3 of (Phase3.Output.t * Phase3.Steps.t)
  | End

type ('output, 'steps) phase =
  | One : (Phase1.Output.t * Phase1.Steps.t) phase
  | Two : (Phase2.Output.t * Phase2.Steps.t) phase
  | Three : (Phase3.Output.t * Phase3.Steps.t) phase

let module_of
  : type o s. (o, s) phase
  -> (module Phase.Steps with type Output.t = o and t = s)
  = function
    | One -> (module Phase1)
    | Two -> (module Phase2)
    | Three -> (module Phase3)

module Make (S : State.S) = struct
  let rec next_of : type s. s -> (_, s) phase -> event =
    fun steps p ->
      let module P = (val module_of p) in
      let module N = Step.Next(P.Steps)(S) in
      match N.value steps, p with
      | Some e, One -> Ph1 e
      | Some e, Two -> Ph2 e
      | Some e, Three -> Ph3 e
      | None, One -> first_of Two
      | None, Two -> first_of Three
      | None, Three -> first_of Two

  and first_of p =
    let module P = (val module_of p) in
    next_of P.steps p
end

module First (S : State.S) = struct
  module M = Make(S)
  let value = M.first_of One
end

module Next (S : State.S) = struct
  let apply output phase =
    let module P = (val module_of phase) in
    let module A = P.Apply(S) in
    match output with
    | Event x -> A.event x
    | Input x -> A.input x
    | Notify _ -> ()

  let is_end output phase =
    let module P = (val module_of phase) in
    P.is_end output

  let next_of steps phase =
    let module M = Make(S) in
    M.next_of steps phase

  let handle : type o, s. (o * s) -> (o, s) phase -> event =
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
