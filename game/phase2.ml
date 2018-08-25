type event =
  | Blessing of Resource.t
  | Defeat
  | End
  | LeaderNew of Leader.t
  | Mercs of Resource.t * bool
  | Nations of Nation.t list
  | Scout of Scouting.report
  | Starvation of Resource.t
  | Support of Nation.support list
  | Turn of Defs.turn
  | Upkeep of Resource.t

module type S = Phase.S with type event_def := event

module Make (M : State.S) : S = struct
  module Scouts = Scouting.Make (M)

  let first () =
    let x = M.get_turn () + 1 in
    Turn x

  let buy_mercs mercs =
    let res = M.get_res () in
    let new_res = Merc.buy mercs res in
    M.set_res new_res

  let apply = function
    | Blessing res -> M.add_res res
    | Defeat
    | End -> ()
    | LeaderNew ldr -> M.set_ldr ldr
    | Mercs (mercs, accept) ->
        if accept then buy_mercs mercs
    | Nations nats -> M.set_nats nats
    | Scout _ -> ()
    | Starvation res ->
        M.sub_res res;
        M.clr_supp ()
    | Support supp_list ->
        Nation.total_of supp_list
        |> M.add_res
    | Turn x ->
        M.set_turn x;
        M.ldr_tick ();
        M.set_enemies (Enemy.spawn x)
    | Upkeep res -> M.sub_res res

  let ldr_res_bonus () =
    match M.get_ldr () with
    | Some ldr -> Leader.res_bonus_of ldr
    | None -> Resource.empty

  let to_support () =
    let ls = M.get_nats () |> Nation.support_of_list in
    Support (ldr_res_bonus () |> Nation.apply_bonus ls)

  let to_upkeep () =
    let res = M.get_res () in
    let x = Resource.manp2supp res in
    let y = Scouts.get_cost () in
    Upkeep Resource.(x ++ y)

  let check_blessing () =
    let deity = M.get_deity () in
    match Deity.blessing_of deity with
    | Some res -> Blessing res
    | None -> to_support ()

  let check_mercs () =
    match Merc.roll () with
    | Some res -> Mercs (res, false)
    | None -> End

  let check_starvation () =
    let res = Resource.mis_supp (M.get_res ()) in
    if res = Resource.empty
    then Starvation Resource.(supp2manp res)
    else Scout (Scouts.get_report ())

  let next = function
    | Turn _ ->
        if M.need_ldr ()
        then LeaderNew (Leader.make ())
        else to_upkeep ()
    | LeaderNew _ -> to_upkeep ()
    | Upkeep _ -> check_starvation ()
    | Starvation _ ->
        if M.has_manp ()
        then Scout (Scouts.get_report ())
        else Defeat
    | Scout _ -> Nations (M.get_nats ())
    | Nations _ -> check_blessing ()
    | Blessing _ -> to_support ()
    | Support _ -> check_mercs ()
    | Mercs _
    | Defeat
    | End -> End
end
