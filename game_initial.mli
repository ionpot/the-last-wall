open Game_defs

type resource = Game_resource.t
type support = Game_support.t

type _ event =
  | Deity : deity event
  | End : unit event
  | Nations : nation list event
  | Starting : resource event
  | Support : support list event

module type T = sig
  val first : unit -> 'a event
  val next : 'a event -> 'a -> 'b event
  val outcome : 'a event -> 'a
end

module Make( ) : T
