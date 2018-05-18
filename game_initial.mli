open Game_defs

type resource = Game_resource.t
type support = Game_support.t

type event =
  | Deity of deity
  | End
  | Nations of nation list
  | Starting of resource
  | Support of support list

module type T = Phase with type event_def := event

module Make(State : Game_state.T) : T
