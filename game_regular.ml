open Game_defs

module Enemy = Game_enemy
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
  { mutable loss : manpower;
    mutable seen : enemy list;
    mutable arrived : enemy list
  }

module type T = Phase with type event_def := event

let scouting_cost =
  Resource.make (Resource.Supply 10)

module Make(State : Game_state.T) : T = struct
  let t =
    { loss = 0;
      seen = [];
      arrived = []
    }

  let move_army () =
    t.arrived <- t.seen;
    t.seen <- []

  let first () = Turn

  let apply = function
    | Attack enemies ->
        t.loss <- Enemy.damage enemies
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
    | SendScouts _ -> ()
    | Starvation manp ->
        State.sub_manp manp
    | Support supp_list ->
        State.add_res (Support.total_of supp_list)
    | Turn ->
        State.inc_turn ();
        move_army ();
        t.loss <- 0;
        t.seen <- Enemy.spawn (State.get_turn ())
    | Upkeep res ->
        State.sub_res res;
        t.loss <- State.missing_supp ()

  let to_upkeep () =
    Upkeep (Outcome.upkeep (State.get_manp ()))

  let next_of = function
    | Turn ->
        if State.has_ldr ()
        then to_upkeep ()
        else NewLeader (Leader.make ())
    | NewLeader _ ->
        to_upkeep ()
    | Upkeep _ ->
        if t.loss > 0
        then Starvation t.loss
        else SendScouts false
    | Starvation _ ->
        SendScouts false
    | SendScouts yes ->
        if yes
        then (ScoutsSent scouting_cost)
        else Nations (State.get_nats ())
    | ScoutsSent _ ->
        ScoutsBack (Enemy.scout t.seen)
    | ScoutsBack _ ->
        Nations (State.get_nats ())
    | Nations _ ->
        Blessing (Outcome.blessing (State.get_deity ()))
    | Blessing _ ->
        Support (Support.of_nats (State.get_nats ()))
    | Support _ ->
        if t.arrived = []
        then Turn
        else Attack t.arrived
    | Attack _ ->
        if t.loss > 0
        then Casualty t.loss
        else Turn
    | Casualty _ ->
        if Leader.lives ()
        then Turn
        else LeaderDied (State.get_ldr ())
    | LeaderDied _ -> Turn
    | End -> End

  let next ev =
    if State.no_manp ()
    then End
    else
      apply ev;
      next_of ev
end
