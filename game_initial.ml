open Game_defs

module O = Game_outcomes
module R = Resource

type _ event =
  | Blessing : resource event
  | Deity : unit event
  | End : unit event
  | Nations : unit event
  | Starting : resource event
  | Support : resource event

type input =
  | Deity of deity
  | Nations of nation list

module Make( ) : T = struct
  type wall =
    { mutable deity : deity;
      mutable nations : nation list;
      mutable res : resource;
    }

  let first : _ event = Deity
  let leader = Alive

  let wall =
    { deity = None;
      nations = [];
      res = R.make ();
    }

  let apply (type a) (ev : a event) (x : a) =
    match ev with
    | Blessing
    | Starting
    | Support ->
        wall.res <- R.add x wall.res
    | Deity
    | End
    | Nations -> ()

  let get_deity () = wall.deity
  let get_leader () = leader
  let get_manpower () = R.manp wall.res
  let get_nations () = wall.nations
  let get_resources () = wall.res
  let get_supplies () = R.supp wall.res

  let next = function
    | Deity -> Starting
    | Starting -> Nations
    | Nations -> Blessing
    | Blessing -> Support
    | Support -> End
    | End -> End

  let outcome_of = function
    | Blessing ->
        O.blessing wall.deity
    | Deity
    | End
    | Nations -> ()
    | Starting ->
        O.starting ()
    | Support ->
        O.support ()

  let set = function
    | Deity d ->
        wall.deity <- d
    | Nations ns ->
        wall.nations <- ns
end

module Trans(X : T) : Transition = struct
  let deity = X.get_deity ()
  let leader = X.get_leader ()
  let nations = X.get_nations ()
  let resource = X.get_resources ()
end
