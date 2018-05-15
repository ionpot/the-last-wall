open Game_defs

type _ event =
  | Blessing : resource event
  | Deity : unit event
  | End : unit event
  | Nations : unit event
  | Starting : resource event
  | Support : resource option event

type input =
  | Deity of deity
  | Nations of nation list

module type T = Phase with
  type 'a event = 'a event and
  type input = input

module Make( ) : T
module Trans(X : T) : Transition
