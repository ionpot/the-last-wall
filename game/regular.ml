open Defs

type event =
  | Attack of Enemy.party list
  | Blessing of Resource.t
  | Casualty of manpower
  | End
  | LeaderDied of Leader.t
  | LeaderLvup of Leader.t
  | LeaderNew of Leader.t
  | Nations of Nation.t list
  | ScoutReport of Scout.report
  | ScoutSumReport of Scout.sum_report
  | ScoutsSent of Resource.t
  | SendScouts of bool
  | Smite of Enemy.party
  | Starvation of manpower
  | Support of Nation.support list
  | Turn of turn
  | Upkeep of supply

module type T = Phase with type event_def := event

module Make(M : State.T) : T = struct
  let next_turn () =
    let x = M.get_turn () + 1 in
    Turn x

  let can_new_ldr () =
    if M.ldr_alive ()
    then false
    else
      M.get_turn () - M.ldr_died_at () > 1

  let ldr_res_bonus () =
    if M.ldr_alive ()
    then M.get_ldr() |> Leader.res_bonus_of
    else Resource.(make Empty)

  let mitigate loss =
    let x = M.get_ldr () |> Leader.mitigate loss in
    loss - x

  let casualty_from enemies =
    let loss = Enemy.damage enemies in
    if M.ldr_alive () then mitigate loss else loss

  let first () =
    next_turn ()

  let apply = function
    | Attack enemies -> ()
    | Blessing res ->
        M.add_res res
    | Casualty manp ->
        M.sub_manp manp;
        M.ldr_won ()
    | End -> ()
    | LeaderDied _ ->
        M.ldr_died ()
    | LeaderLvup ldr
    | LeaderNew ldr ->
        M.set_ldr ldr
    | Nations nats ->
        M.set_nats nats
    | ScoutReport _
    | ScoutSumReport _ -> ()
    | ScoutsSent res ->
        M.sub_res res
    | SendScouts yes ->
        M.set_scouting yes
    | Smite x ->
        M.get_enemies ()
        |> Enemy.reduce x
        |> M.set_enemies
    | Starvation manp ->
        M.sub_manp manp;
        M.clr_supp ()
    | Support supp_list ->
        Nation.total_of supp_list
        |> M.add_res
    | Turn x ->
        M.set_turn x;
        Enemy.spawn x |> M.set_enemies
    | Upkeep sup ->
        M.sub_supp sup

  let to_support () =
    let ls = M.get_nats () |> Nation.support_of_list in
    Support (ldr_res_bonus () |> Nation.apply_bonus ls)

  let to_upkeep () =
    Upkeep (M.get_manp ())

  let ask_scouting () =
    SendScouts (M.is_scouting ())

  let check_blessing () =
    match M.get_deity () |> Deity.blessing_of with
    | Some x -> Blessing x
    | None -> to_support ()

  let check_casualty enemies =
    let loss = casualty_from enemies in
    if loss > 0
    then Casualty loss
    else next_turn ()

  let check_smite enemies =
    match Enemy.smite enemies with
    | Some x -> Smite x
    | None -> check_casualty enemies

  let check_lvup ldr =
    if Leader.can_lvup ldr
    then LeaderLvup (Leader.lvup ldr)
    else ask_scouting ()

  let check_scouting () =
    if M.is_scouting ()
    then ScoutsSent Scout.cost
    else to_upkeep ()

  let check_report () =
    let e = M.get_enemies () in
    if M.is_scouting ()
    then ScoutReport (Scout.report_of e)
    else ScoutSumReport (Scout.sum_report_of e)

  let next_of = function
    | Turn _ ->
        if can_new_ldr ()
        then LeaderNew (Leader.make ())
        else check_scouting ()
    | LeaderNew _ ->
        check_scouting ()
    | ScoutsSent _ ->
        to_upkeep ()
    | Upkeep _ ->
        let loss = M.missing_supp () in
        if loss > 0
        then Starvation loss
        else check_report ()
    | Starvation _ ->
        check_report ()
    | ScoutReport _
    | ScoutSumReport _ ->
        Nations (M.get_nats ())
    | Nations _ ->
        check_blessing ()
    | Blessing _ ->
        to_support ()
    | Support _ ->
        let enemies = M.get_enemies () in
        if enemies = []
        then ask_scouting ()
        else Attack enemies
    | Attack enemies ->
        if M.get_deity () = Deity.Lerota
        then check_smite enemies
        else check_casualty enemies
    | Smite _ ->
        M.get_enemies ()
        |> check_casualty
    | Casualty _ ->
        if M.ldr_alive ()
        then
          let ldr = M.get_ldr () in
          if Leader.lives ()
          then check_lvup ldr
          else LeaderDied ldr
        else ask_scouting ()
    | LeaderLvup _
    | LeaderDied _ ->
        ask_scouting ()
    | SendScouts _ -> next_turn ()
    | End -> End

  let next ev =
    apply ev;
    if M.no_manp ()
    then End
    else next_of ev
end
