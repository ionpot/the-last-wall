type event =
  | Deity of Deity.t
  | End
  | Nations of Nation.t list
  | SendScouts of bool
  | Starting of Resource.t
  | Support of Nation.support list

module type T = Defs.Phase with type event_def := event

module Make(M : State.T) : T = struct
  let first () = Deity (M.get_deity ())

  let apply = function
    | Deity x -> M.set_deity x
    | End -> ()
    | Nations x -> M.set_nats x
    | SendScouts x -> M.set_scouting x
    | Starting x -> M.add_res x
    | Support x -> M.add_res (Nation.total_of x)

  let next_of = function
    | Deity _ -> Starting (Deity.starting (M.get_deity ()))
    | Starting _ -> Nations (M.get_nats ())
    | Nations _ -> SendScouts (M.is_scouting ())
    | SendScouts _ -> Support (Nation.support_of_list (M.get_nats ()))
    | Support _ -> End
    | End -> End

  let next ev =
    apply ev;
    next_of ev
end