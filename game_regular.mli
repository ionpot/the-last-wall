open Game_defs

type enemy = Game_enemy.party
type leader = Game_leader.t
type resource = Game_resource.t
type support = Game_support.t

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

module Make(State : Game_state.T) : T
