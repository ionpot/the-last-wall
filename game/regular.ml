open Defs

type enemy = Enemy.party
type leader = Leader.t
type nation = Nation.t
type resource = Resource.t
type support = Nation.support

type event =
  | Attack of enemy list
  | Blessing of resource
  | Casualty of manpower
  | End
  | LeaderDied of leader
  | LeaderLvup of leader
  | Nations of nation list
  | NewLeader of leader
  | ScoutsBack of enemy list
  | ScoutsSent of resource
  | SendScouts of bool
  | Starvation of manpower
  | Support of support list
  | Turn of turn
  | Upkeep of resource

module type T = Phase with type event_def := event

let upkeep mp =
  Resource.(make (Supply mp))

module Make(M : State.T) : T = struct
  let next_turn () =
    let x = M.get_turn () + 1 in
    Turn x

  let can_new_ldr () =
    if M.ldr_alive ()
    then false
    else
      M.get_turn () - M.ldr_died_at () > 1

  let mitigate loss =
    let lv = M.get_ldr () |> Leader.level_of in
    let x = 0.1 +. (0.01 *. float lv) in
    loss - truncate (x *. float loss)

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
    | LeaderLvup ldr ->
        M.set_ldr ldr
    | Nations nats ->
        M.set_nats nats
    | NewLeader ldr ->
        M.set_ldr ldr
    | ScoutsBack _ -> ()
    | ScoutsSent res ->
        M.sub_res res
    | SendScouts yes ->
        M.set_scouting yes
    | Starvation manp ->
        M.sub_manp manp;
        M.clr_supp ()
    | Support supp_list ->
        Nation.total_of supp_list
        |> M.add_res
    | Turn x ->
        M.set_turn x;
        Enemy.spawn x |> M.set_enemies
    | Upkeep res ->
        M.sub_res res

  let to_upkeep () =
    Upkeep (upkeep (M.get_manp ()))

  let ask_scouting () =
    SendScouts (M.is_scouting ())

  let check_lvup ldr =
    if Leader.can_lvup ldr
    then LeaderLvup (Leader.lvup ldr)
    else ask_scouting ()

  let check_scouting () =
    if M.is_scouting ()
    then ScoutsSent Enemy.scouting_cost
    else to_upkeep ()

  let check_report () =
    let report =
      let enemies = M.get_enemies () in
      if M.is_scouting ()
      then Enemy.scout enemies
      else Enemy.vague_scout enemies
    in
    ScoutsBack report

  let next_of = function
    | Turn _ ->
        if can_new_ldr ()
        then NewLeader (Leader.make ())
        else check_scouting ()
    | NewLeader _ ->
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
    | ScoutsBack _ ->
        Nations (M.get_nats ())
    | Nations _ ->
        Blessing (Deity.blessing_of (M.get_deity ()))
    | Blessing _ ->
        Support (Nation.support_of_list (M.get_nats ()))
    | Support _ ->
        let enemies = M.get_enemies () in
        if enemies = []
        then ask_scouting ()
        else Attack enemies
    | Attack enemies ->
        let loss = casualty_from enemies in
        if loss > 0
        then Casualty loss
        else next_turn ()
    | Casualty _ ->
        let ldr = M.get_ldr () in
        if Leader.lives ()
        then check_lvup ldr
        else LeaderDied ldr
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
