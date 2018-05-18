open Game_defs

type resource = Game_resource.t
type support = Game_support.t

type events =
  | Deity of deity
  | End
  | Nations of nation list
  | Starting of resource
  | Support of support list

module type T = Phase with type event := events

module Make(State : Game_state.T) : T
