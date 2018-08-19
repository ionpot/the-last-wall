open Defs

type event =
  | Attack of Enemy.party list
  | Blessing of Resource.t
  | Casualty of Resource.t
  | End
  | LeaderDied of Leader.t
  | LeaderLvup of Leader.t
  | LeaderNew of Leader.t
  | Mercs of Resource.t * bool
  | Nations of Nation.t list
  | ScoutReport of Enemy.report
  | ScoutSumReport of Enemy.sum_report
  | SendScouts of bool
  | Smite of Enemy.party
  | Starvation of Resource.t
  | Support of Nation.support list
  | Turn of turn
  | Upkeep of Resource.t

module type S = Phase with type event_def := event

module Make(M : State.S) : S = struct
  let next_turn () =
    let x = M.get_turn () + 1 in
    Turn x

  let scouting_cost () =
    if M.is_scouting ()
    then Resource.of_supp 10
    else Resource.empty

  let can_new_ldr () =
    if M.ldr_alive ()
    then false
    else
      M.get_turn () - M.ldr_died_at () > 1

  let ldr_res_bonus () =
    if M.ldr_alive ()
    then M.get_ldr() |> Leader.res_bonus_of
    else Resource.empty

  let mitigate loss =
    let x = M.get_ldr () |> Leader.mitigate loss in
    Resource.(loss -- x)

  let casualty_from enemies =
    let loss = Enemy.damage enemies in
    if M.ldr_alive () then mitigate loss else loss

  let buy_mercs res =
    let cost = Resource.cost_of res in
    M.sub_res cost;
    M.add_res res

  let first () =
    next_turn ()

  let apply = function
    | Attack enemies -> ()
    | Blessing res -> M.add_res res
    | Casualty res ->
        M.sub_res res;
        M.ldr_won ()
    | End -> ()
    | LeaderDied _ -> M.ldr_died ()
    | LeaderLvup ldr
    | LeaderNew ldr -> M.set_ldr ldr
    | Mercs (res, accept) ->
        if accept then buy_mercs res
    | Nations nats -> M.set_nats nats
    | ScoutReport _
    | ScoutSumReport _ -> ()
    | SendScouts yes -> M.set_scouting yes
    | Smite party ->
        M.get_enemies ()
        |> Enemy.reduce party
        |> M.set_enemies
    | Starvation res ->
        M.sub_res res;
        M.clr_supp ()
    | Support supp_list ->
        Nation.total_of supp_list
        |> M.add_res
    | Turn x ->
        M.set_turn x;
        Enemy.spawn x |> M.set_enemies
    | Upkeep res -> M.sub_res res

  let to_support () =
    let ls = M.get_nats () |> Nation.support_of_list in
    Support (ldr_res_bonus () |> Nation.apply_bonus ls)

  let to_upkeep () =
    let res = M.get_res () in
    let x = Resource.cost_of res in
    let y = scouting_cost () in
    Upkeep Resource.(x ++ y)

  let ask_scouting () =
    SendScouts (M.is_scouting ())

  let check_atk () =
    let enemies = M.get_enemies () in
    if enemies = []
    then ask_scouting ()
    else Attack enemies

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
        else check_atk ()
    | None -> check_atk ()

  let to_casualty enemies =
    let loss = casualty_from enemies in
    Casualty loss

  let check_smite enemies =
    match Enemy.smite enemies with
    | Some party -> Smite party
    | None -> to_casualty enemies

  let check_lvup ldr =
    if Leader.can_lvup ldr
    then LeaderLvup (Leader.lvup ldr)
    else ask_scouting ()

  let check_ldr () =
    if M.ldr_alive ()
    then
      let ldr = M.get_ldr () in
      if Leader.lives ()
      then check_lvup ldr
      else LeaderDied ldr
    else ask_scouting ()

  let check_report () =
    let e = M.get_enemies () in
    if M.is_scouting ()
    then ScoutReport (Enemy.report_of e)
    else ScoutSumReport (Enemy.sum_report_of e)

  let check_starvation () =
    match M.missing_supp () with
    | Some loss -> Starvation loss
    | None -> check_report ()

  let next_of = function
    | Turn _ ->
        if can_new_ldr ()
        then LeaderNew (Leader.make ())
        else to_upkeep ()
    | LeaderNew _ -> to_upkeep ()
    | Upkeep _ -> check_starvation ()
    | Starvation _ -> check_report ()
    | ScoutReport _
    | ScoutSumReport _ -> Nations (M.get_nats ())
    | Nations _ -> check_blessing ()
    | Blessing _ -> to_support ()
    | Support _ -> check_mercs ()
    | Mercs _ -> check_atk ()
    | Attack enemies ->
        if M.get_deity () = Deity.Lerota
        then check_smite enemies
        else to_casualty enemies
    | Smite _ -> to_casualty @@ M.get_enemies ()
    | Casualty res ->
        if Resource.has_manp res
        then check_ldr ()
        else ask_scouting ()
    | LeaderLvup _
    | LeaderDied _ -> ask_scouting ()
    | SendScouts _ -> next_turn ()
    | End -> End

  let next ev =
    apply ev;
    if M.has_manp ()
    then next_of ev
    else End
end
