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

  let first_state = Deity

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
    | End
    | Nations -> ()

  let get = function
    | Leader -> wall.leader
    | Manpower -> fst wall.res
    | Nations -> wall.nations
    | Supplies -> snd wall.res

  let next = function
    | Blessing -> Support
    | Deity -> Starting
    | End -> End
    | Nations -> Blessing
    | Starting -> Nations
    | Support -> End

  let set = function
    | Deity d ->
        wall.deity <- d
    | Nations ns ->
        wall.nations <- ns
end

module Trans(X : T) : Transition = struct
  val deity : deity
  val leader : leader
  val nations : nation list
  val resource : resource
end
