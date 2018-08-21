type event =
  | Blessing of Resource.t
  | Defeat
  | End
  | LeaderNew of Leader.t
  | Mercs of Resource.t * bool
  | Nations of Nation.t list
  | ScoutReport of Enemy.report
  | ScoutSumReport of Enemy.sum_report
  | Starvation of Resource.t
  | Support of Nation.support list
  | Turn of Defs.turn
  | Upkeep of Resource.t

module type S = Phase.S with type event_def := event

module Make(M : State.S) : S = struct
  let next_turn () =
    let x = M.get_turn () + 1 in
    Turn x

  let first () =
    next_turn ()

  let buy_mercs res =
    let cost = Resource.cost_of res in
    M.sub_res cost;
    M.add_res res

  let apply = function
    | Blessing res -> M.add_res res
    | Defeat
    | End -> ()
    | LeaderNew ldr -> M.set_ldr ldr
    | Mercs (res, accept) ->
        if accept then buy_mercs res
    | Nations nats -> M.set_nats nats
    | ScoutReport _
    | ScoutSumReport _ -> ()
    | Starvation res ->
        M.sub_res res;
        M.clr_supp ()
    | Support supp_list ->
        Nation.total_of supp_list
        |> M.add_res
    | Turn x ->
        M.set_turn x;
        M.set_enemies (Enemy.spawn x)
    | Upkeep res -> M.sub_res res

  let can_new_ldr () =
    if M.ldr_alive ()
    then false
    else
      M.get_turn () - M.ldr_died_at () > 1

  let ldr_res_bonus () =
    if M.ldr_alive ()
    then M.get_ldr() |> Leader.res_bonus_of
    else Resource.empty

  let to_support () =
    let ls = M.get_nats () |> Nation.support_of_list in
    Support (ldr_res_bonus () |> Nation.apply_bonus ls)

  let scouting_cost () =
    if M.is_scouting ()
    then Resource.of_supp 10
    else Resource.empty

  let to_upkeep () =
    let res = M.get_res () in
    let x = Resource.cost_of res in
    let y = scouting_cost () in
    Upkeep Resource.(x ++ y)

  let check_blessing () =
    let deity = M.get_deity () in
    match Deity.blessing_of deity with
    | Some res -> Blessing res
    | None -> to_support ()

  let check_mercs () =
    match Merc.roll () with
    | Some res ->
        if M.can_afford res
        then Mercs (res, false)
        else End
    | None -> End

  let check_report () =
    let e = M.get_enemies () in
    if M.is_scouting ()
    then ScoutReport (Enemy.report_of e)
    else ScoutSumReport (Enemy.sum_report_of e)

  let check_starvation () =
    match M.missing_supp () with
    | Some loss -> Starvation loss
    | None -> check_report ()

  let next = function
    | Turn _ ->
        if can_new_ldr ()
        then LeaderNew (Leader.make ())
        else to_upkeep ()
    | LeaderNew _ -> to_upkeep ()
    | Upkeep _ -> check_starvation ()
    | Starvation _ ->
        if M.has_manp ()
        then check_report ()
        else Defeat
    | ScoutReport _
    | ScoutSumReport _ -> Nations (M.get_nats ())
    | Nations _ -> check_blessing ()
    | Blessing _ -> to_support ()
    | Support _ -> check_mercs ()
    | Mercs _
    | Defeat
    | End -> End
end
