type event =
  | Ph1 of Step.Of(Phase1).t
  | Ph2 of Step.Of(Phase2).t
  | Ph3 of Step.Of(Phase1).t

type step =
  | Next of event
  | EndOf of Phase.t

module Step (State : State.S) (Phase : Phase.S) = struct
  module Step = Step.Of(Phase)
  module Do = Step.Do(State)

  let apply (event, _) = Do.apply event

  let make steps phase next =
    match Do.next steps with
    | Some event -> Next (next event)
    | None -> EndOf phase

  let first_of = make Step.first
  let next_of (_, steps) = make steps
end

module Handle (State : State.S) = struct
  module Step = Step(State)

  let first_of phase =
    match phase with
    | Phase.One ->
        let module Step = Step(Phase1) in
        Step.first_of phase (fun event -> Ph1 event)
    | Phase.Two ->
        let module Step = Step(Phase2) in
        Step.first_of phase (fun event -> Ph2 event)
    | Phase.Three ->
        let module Step = Step(Phase1) in
        Step.first_of phase (fun event -> Ph3 event)

  let next_of = function
    | Ph1 event ->
        let module Step = Step(Phase1) in
        Step.next_of event Phase.One (fun event -> Ph1 event)
    | Ph2 event ->
        let module Step = Step(Phase2) in
        Step.next_of event Phase.Two (fun event -> Ph2 event)
    | Ph3 event ->
        let module Step = Step(Phase1) in
        Step.next_of event Phase.Three (fun event -> Ph3 event)

  let apply = function
    | Ph1 event -> let module Step = Step(Phase1) in Step.apply event
    | Ph2 event -> let module Step = Step(Phase2) in Step.apply event
    | Ph3 event -> let module Step = Step(Phase1) in Step.apply event
end
