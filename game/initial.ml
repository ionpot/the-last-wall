type event =
  | Deity of Deity.t
  | End
  | Nations of Nation.t list
  | Starting of Resource.t
  | Support of Nation.support list

module type T = Defs.Phase with type event_def := event

module Make(M : State.T) : T = struct
  let first () = Deity (M.get_deity ())

  let apply = function
    | Starting x -> M.add_res x
    | Support x -> M.add_res (Nation.total_of x)
    | Deity x -> M.set_deity x
    | End -> ()
    | Nations x -> M.set_nats x

  let next_of = function
    | Deity _ -> Starting (Deity.starting (M.get_deity ()))
    | Starting _ -> Nations (M.get_nats ())
    | Nations _ -> Support (Nation.support_of_list (M.get_nats ()))
    | Support _ -> End
    | End -> End

  let next ev =
    apply ev;
    next_of ev
end
