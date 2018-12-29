type event =
  | Build of Building.t list
  | BuildSupply of Defs.supply
  | End
  | Nations of Nation.t list
  | SendScouts of bool
  | Starting of Resource.t
  | Support of Nation.support list

module type S = Phase.S with type event_def := event

module Make (M : State.S) : S = struct
  module Support = Check_support.Make(M)

  let first () =
    let deity = M.get_deity () in
    Starting (Deity.starting deity)

  let apply = function
    | Build x -> M.build x
    | BuildSupply x -> M.bld_supp x
    | End -> ()
    | Nations x -> M.set_nats x
    | SendScouts x -> M.set_scouting x
    | Starting x -> M.add_res x
    | Support x -> M.add_res (Nation.total_of x)

  let check_supp () =
    let cost = M.bld_supp_cost () in
    if cost > 0
    then BuildSupply cost
    else End

  let next = function
    | Starting _ -> Nations (M.get_nats ())
    | Nations _ -> Support (Support.get ())
    | Support _ -> Build []
    | Build _ -> check_supp ()
    | BuildSupply _ -> SendScouts (M.is_scouting ())
    | SendScouts _
    | End -> End
end
