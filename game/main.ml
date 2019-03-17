type t =
  | Event of Phases.event
  | End

module Make (State : State.S) = struct
  module Handle = Phases.Handle(State)

  let transition = function
    | Phase.One -> Phase.Two
    | Phase.Two -> Phase.Three
    | Phase.Three -> Phase.Two

  let rec first_of phase =
    check (Handle.first_of phase)

  and check = function
    | Phases.Next event -> Event event
    | Phases.EndOf phase -> first_of (transition phase)
    | Phases.End -> End

  let next = function
    | End -> End
    | Event e ->
        Handle.apply e;
        check (Handle.next_of e)

  let first () =
    first_of Phase.One
end
