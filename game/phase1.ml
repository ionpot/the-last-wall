type event =
  | End
  | Nations of Nation.t list
  | SendScouts of bool
  | Starting of Resource.t
  | Support of Nation.support list

module type S = Phase.S with type event_def := event

module Make (M : State.S) : S = struct
  module Support = Check_support.Make (M)

  let first () =
    let deity = M.get_deity () in
    Starting (Deity.starting deity)

  let apply = function
    | End -> ()
    | Nations x -> M.set_nats x
    | SendScouts x -> M.set_scouting x
    | Starting x -> M.add_res x
    | Support x -> M.add_res (Nation.total_of x)

  let next = function
    | Starting _ -> Nations (M.get_nats ())
    | Nations _ -> SendScouts (M.is_scouting ())
    | SendScouts _ -> Support (Support.get ())
    | Support _
    | End -> End
end
