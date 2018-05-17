open Game_defs

module O = Game_outcomes
module R = Game_resource
module S = Game_support

type resource = R.t
type support = S.t

type event =
  | Deity of deity
  | End
  | Nations of nation list
  | Starting of resource
  | Support of support list

module type T = sig
  val first : unit -> event
  val next : event -> event
end

module Make( ) : T = struct
  type t =
    { mutable deity : deity;
      mutable nations : nation list;
      mutable res : resource
    }

  let wall =
    { deity = NoDeity;
      nations = [];
      res = R.make R.Empty
    }

  let first () = Deity wall.deity

  let apply ev =
    let open R in
    match ev with
    | Starting x ->
        wall.res <- wall.res ++ x
    | Support x ->
        wall.res <- wall.res ++ S.total_of x
    | Deity x ->
        wall.deity <- x
    | End -> ()
    | Nations x ->
        wall.nations <- x

  let next_of = function
    | Deity _ -> Starting (O.starting wall.deity)
    | Starting _ -> Nations wall.nations
    | Nations _ -> Support (S.of_list wall.nations)
    | Support _ -> End
    | End -> End

  let next ev =
    apply ev;
    next_of ev
end
