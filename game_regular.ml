open Game_defs

module Enemy = Game_enemy
module Garrison = Game_garrison
module Leader = Game_leader
module Outcome = Game_outcome
module Resource = Game_resource
module Support = Game_support

type enemy = Enemy.party
type leader = Leader.t
type resource = Resource.t
type support = Support.t

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

type t =
  { mutable seen : enemy list;
    mutable arrived : enemy list
  }

module type T = Phase with type event_def := event

module Make(State : Game_state.T) : T = struct
  let t =
    { seen = [];
      arrived = []
    }

  let first () = Turn

  let apply = function
    | Attack enemies -> ()
    | Blessing res ->
        State.add_res res
    | Casualty manp ->
        State.sub_manp manp
    | End -> ()
    | LeaderDied _ ->
        State.clr_ldr ()
    | Nations nats ->
        State.set_nats nats
    | NewLeader ldr ->
        State.set_ldr ldr
    | ScoutsBack _ -> ()
    | ScoutsSent res ->
        State.sub_res res
    | SendScouts yes ->
        State.set_scouting yes
    | Starvation manp ->
        State.sub_manp manp
    | Support supp_list ->
        State.add_res (Support.total_of supp_list)
    | Turn ->
        State.inc_turn ();
        t.arrived <- t.seen;
        t.seen <- Enemy.spawn (State.get_turn ())
    | Upkeep res ->
        State.sub_res res

  let to_upkeep () =
    Upkeep (Outcome.upkeep (State.get_manp ()))

  let ask_scouting () =
    SendScouts (State.is_scouting ())

  let check_scouting () =
    if State.is_scouting ()
    then ScoutsSent Enemy.scouting_cost
    else to_upkeep ()

  let check_report () =
    if State.is_scouting ()
    then ScoutsBack (Enemy.scout t.seen)
    else ScoutsBack (Enemy.vague_scout t.seen)

  let get_casualty enemies =
    let loss = Enemy.damage enemies in
    State.get_garrison ()
    |> Garrison.mitigate loss

  let next_of = function
    | Turn ->
        if State.has_ldr ()
        then check_scouting ()
        else NewLeader (Leader.make ())
    | NewLeader _ ->
        check_scouting ()
    | ScoutsSent _ ->
        to_upkeep ()
    | Upkeep _ ->
        let loss = State.missing_supp () in
        if loss > 0
        then Starvation loss
        else check_report ()
    | Starvation _ ->
        check_report ()
    | ScoutsBack _ ->
        Nations (State.get_nats ())
    | Nations _ ->
        Blessing (Outcome.blessing (State.get_deity ()))
    | Blessing _ ->
        Support (Support.of_nats (State.get_nats ()))
    | Support _ ->
        if t.arrived = []
        then ask_scouting ()
        else Attack t.arrived
    | Attack enemies ->
        let loss = get_casualty enemies in
        if loss > 0
        then Casualty loss
        else Turn
    | Casualty _ ->
        if Leader.lives ()
        then ask_scouting ()
        else LeaderDied (State.get_ldr ())
    | LeaderDied _ ->
        ask_scouting ()
    | SendScouts _ -> Turn
    | End -> End

  let next ev =
    if State.no_manp ()
    then End
    else
      apply ev;
      next_of ev
end
