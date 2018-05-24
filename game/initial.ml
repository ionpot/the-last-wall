open Defs

type resource = Resource.t
type support = Support.t

type event =
  | Deity of deity
  | End
  | Nations of nation list
  | Starting of resource
  | Support of support list

module type T = Phase with type event_def := event

module Make(M : State.T) : T = struct
  let first () = Deity (M.get_deity ())

  let apply = function
    | Starting x -> M.add_res x
    | Support x -> M.add_res (Support.total_of x)
    | Deity x -> M.set_deity x
    | End -> ()
    | Nations x -> M.set_nats x

  let next_of = function
    | Deity _ -> Starting (Outcome.starting (M.get_deity ()))
    | Starting _ -> Nations (M.get_nats ())
    | Nations _ -> Support (Support.of_nats (M.get_nats ()))
    | Support _ -> End
    | End -> End

  let next ev =
    apply ev;
    next_of ev
end
