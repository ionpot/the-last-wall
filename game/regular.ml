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
  | Nations of nation list
  | NewLeader of leader
  | ScoutsBack of enemy list
  | ScoutsSent of resource
  | SendScouts of bool
  | Starvation of manpower
  | Support of support list
  | Turn
  | Upkeep of resource

module type T = Phase with type event_def := event

let mitigate loss = function
  | Some ldr ->
      let lv = Leader.level_of ldr in
      let x = 0.1 +. (0.01 *. float lv) in
      loss - truncate (x *. float loss)
  | None -> loss

let upkeep mp =
  Resource.(make (Supply mp))

module Make(M : State.T) : T = struct
  let first () = Turn

  let apply = function
    | Attack enemies -> ()
    | Blessing res ->
        M.add_res res
    | Casualty manp ->
        M.sub_manp manp
    | End -> ()
    | LeaderDied _ ->
        M.clr_ldr ()
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
        M.sub_manp manp
    | Support supp_list ->
        M.add_res (Nation.total_of supp_list)
    | Turn ->
        M.inc_turn ();
        M.get_turn () |> Enemy.spawn |> M.move_enemies
    | Upkeep res ->
        M.sub_res res

  let to_upkeep () =
    Upkeep (upkeep (M.get_manp ()))

  let ask_scouting () =
    SendScouts (M.is_scouting ())

  let leader_died () =
    match M.get_ldr () with
    | Some x -> LeaderDied x
    | None -> ask_scouting ()

  let check_scouting () =
    if M.is_scouting ()
    then ScoutsSent Enemy.scouting_cost
    else to_upkeep ()

  let check_report () =
    let seen = M.get_seen () in
    if M.is_scouting ()
    then ScoutsBack (Enemy.scout seen)
    else ScoutsBack (Enemy.vague_scout seen)

  let casualty_from enemies =
    let loss = Enemy.damage enemies in
    M.get_ldr () |> mitigate loss

  let next_of = function
    | Turn ->
        if M.has_ldr ()
        then check_scouting ()
        else NewLeader (Leader.make ())
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
        let arrived = M.get_arrived () in
        if arrived = []
        then ask_scouting ()
        else Attack arrived
    | Attack enemies ->
        let loss = casualty_from enemies in
        if loss > 0
        then Casualty loss
        else Turn
    | Casualty _ ->
        if Leader.lives ()
        then ask_scouting ()
        else leader_died ()
    | LeaderDied _ ->
        ask_scouting ()
    | SendScouts _ -> Turn
    | End -> End

  let next ev =
    if M.no_manp ()
    then End
    else
      apply ev;
      next_of ev
end
