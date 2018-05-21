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
  | Starvation of resource
  | Support of support list
  | Turn
  | Upkeep of resource

module type T = Phase with type event_def := event

module Make(State : Game_state.T) : T
