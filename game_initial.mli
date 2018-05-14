open Game_defs

module type T = Phase with
  type _ event =
    Blessing : resource event
    Deity : unit event
    Nations : unit event
    Starting : resource event
    Support : resource event
  and
  type input =
    Deity of deity
    Nations of nation list

module Make( ) : T
module Trans(X : T) : Transition
