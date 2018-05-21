open Game_defs

type party = Game_enemy.party
type resource = Game_resource.t
type support = Game_support.t

type event =
  | Attack of party list
  | Blessing of resource
  | Casualty of (manpower * leader)
  | End
  | Nations of nation list
  | Scout of party list
  | Starvation of manpower
  | Support of support list
  | Turn
  | Upkeep of resource

type t =
  { mutable loss : manpower;
    mutable seen : party list
    mutable arrived : party list
  }

module type T = Phase with type event_def := event

module Make(State : Game_state.T) : T = struct
  module Enemy = Game_enemy
  module Outcome = Game_outcome
  module Support = Game_support

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
    | Attack xs ->
        t.loss <- Enemy.damage xs
    | Blessing res ->
        State.add_res res
    | Casualty (manp * leader) ->
        State.sub_manp manp;
        State.set_ldr leader
    | End -> ()
    | Nations nats ->
        State.set_nats nats
    | Scout xs ->
        t.seen <- xs
    | Starvation manp ->
        State.sub_manp manp
    | Support xs ->
        State.add_res (Support.total_of xs)
    | Turn ->
        t.loss <- 0;
        move_army ()
    | Upkeep res ->
        State.sub_res res;
        t.loss <- State.missing_supp ()

  let next_of = function
    | Turn ->
        Scout (Enemy.spawn (State.get_turn ()))
    | Scout _ ->
        Upkeep (Outcome.upkeep (State.get_manp ()))
    | Upkeep _ ->
        if t.loss > 0
        then Starvation t.loss
        else Nations (State.get_nats ())
    | Starvation _ ->
        Nations (State.get_nats ())
    | Nations _ ->
        Blessing (Outcome.blessing (State.get_deity ()))
    | Blessing _ ->
        Support (Support.of_nats (State.get_nats ()))
    | Support _ ->
        if t.arrived = []
        then Turn
        else Attack t.arrived
    | Attack a ->
        if t.loss > 0
        then Casualty (t.loss, Alive)
        else Turn
    | Casualty _ -> Turn
    | End -> End

  let next ev =
    if State.no_manp ()
    then End
    else
      apply ev;
      next_of ev
end
