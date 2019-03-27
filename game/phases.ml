type event =
  | Ph1 of Step.Of(Phase1.S).event
  | Ph2 of Step.Of(Phase2.S).event
  | Ph3 of Step.Of(Phase3.S).event

type step =
  | Next of event
  | EndOf of Phase.t

module Step (State : State.S) (Phase : Phase.S) = struct
  module Step = Step.Of(Phase)
  module Do = Step.Do(State)

  let apply = Do.apply

  let first_of phase next =
    match Do.first () with
    | Some event -> Next (next event)
    | None -> EndOf phase

  let next_of step phase next =
    match Do.next step with
    | Some event -> Next (next event)
    | None -> EndOf phase
end

module Handle (State : State.S) = struct
  module Step = Step(State)

  let first_of phase =
    match phase with
    | Phase.One ->
        let module Step = Step(Phase1.S) in
        Step.first_of phase (fun event -> Ph1 event)
    | Phase.Two ->
        let module Step = Step(Phase2.S) in
        Step.first_of phase (fun event -> Ph2 event)
    | Phase.Three ->
        let module Step = Step(Phase3.S) in
        Step.first_of phase (fun event -> Ph3 event)

  let next_of = function
    | Ph1 event ->
        let module Step = Step(Phase1.S) in
        Step.next_of event Phase.One (fun event -> Ph1 event)
    | Ph2 event ->
        let module Step = Step(Phase2.S) in
        Step.next_of event Phase.Two (fun event -> Ph2 event)
    | Ph3 event ->
        let module Step = Step(Phase3.S) in
        Step.next_of event Phase.Three (fun event -> Ph3 event)

  let apply = function
    | Ph1 event -> let module Step = Step(Phase1.S) in Step.apply event
    | Ph2 event -> let module Step = Step(Phase2.S) in Step.apply event
    | Ph3 event -> let module Step = Step(Phase3.S) in Step.apply event
end
