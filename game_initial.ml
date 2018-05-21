open Game_defs

module Outcome = Game_outcome
module Resource = Game_resource
module Support = Game_support

type resource = Resource.t
type support = Support.t

type event =
  | Deity of deity
  | End
  | Nations of nation list
  | Starting of resource
  | Support of support list

module type T = Phase with type event_def := event

module Make(State : Game_state.T) : T = struct
  let first () = Deity (State.get_deity ())

  let apply = function
    | Starting x -> State.add_res x
    | Support x -> State.add_res (Support.total_of x)
    | Deity x -> State.set_deity x
    | End -> ()
    | Nations x -> State.set_nats x

  let next_of = function
    | Deity _ -> Starting (Outcome.starting (State.get_deity ()))
    | Starting _ -> Nations (State.get_nats ())
    | Nations _ -> Support (Support.of_nats (State.get_nats ()))
    | Support _ -> End
    | End -> End

  let next ev =
    apply ev;
    next_of ev
end
