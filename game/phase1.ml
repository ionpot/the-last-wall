type event =
  | End
  | Nations of Nation.t list
  | NewLeader of Leader.t
  | SendScouts of bool
  | Starting of Resource.t
  | Support of Nation.support list

module type S = Phase.S with type event_def := event

module Make (M : State.S) : S = struct
  let first () =
    let deity = M.get_deity () in
    Starting (Deity.starting deity)

  let apply = function
    | End -> ()
    | Nations x -> M.set_nats x
    | NewLeader x -> M.set_ldr x
    | SendScouts x -> M.set_scouting x
    | Starting x -> M.add_res x
    | Support x -> M.add_res (Nation.total_of x)

  let next = function
    | Starting _ -> NewLeader (Leader.random ())
    | NewLeader _ -> Nations (M.get_nats ())
    | Nations _ -> SendScouts (M.is_scouting ())
    | SendScouts _ -> Support (Nation.support_of_list (M.get_nats ()))
    | Support _
    | End -> End
end
