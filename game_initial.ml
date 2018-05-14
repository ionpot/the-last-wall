open Game_defs
open Pair

module Make( ) : T = struct
  type _ event =
    Blessing : resource event
    Deity : unit event
    Nations : unit event
    Starting : resource event
    Support : resource event
  type _ query =
    | Leader : leader query
    | Manpower : manpower query
    | Nations : nation list query
    | Supplies : supply query
  type input =
    Deity of deity
    Nations of nation list
  type wall =
    { mutable deity : deity;
      mutable leader : leader;
      mutable nations : nation list;
      mutable resources : resource;
    }
  let wall =
    { deity = None;
      leader = Alive;
      nations = [];
      res = (0, 0);
    }
  let apply = function
    | Blessing p
    | Starting p
    | Support p ->
        wall.res <- wall.res ++ p
    | Deity
    | Nations -> ()
  val get : type a. a query -> a
  val next : unit -> event
  val set : input -> unit
end

module Trans(X : T) : Transition = struct
  val deity : deity
  val leader : leader
  val nations : nation list
  val resource : resource
end
