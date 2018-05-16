open Game_defs

module O = Game_outcomes
module R = Game_resource
module S = Game_support

type resource = R.t
type support = S.t

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

  let first () = Deity

  let outcome : type a. a event -> a =
    function
      | Deity -> wall.deity
      | End -> ()
      | Nations -> wall.nations
      | Starting -> O.starting wall.deity
      | Support -> S.of_list wall.nations

  let apply : type a. a event -> a -> unit =
    fun ev x ->
      let open R in
      match ev with
      | Starting ->
          wall.res <- wall.res ++ x
      | Support ->
          wall.res <- wall.res ++ S.total_of x
      | Deity ->
          wall.deity <- x
      | End -> ()
      | Nations ->
          wall.nations <- x

  let next_of : type a b. a event -> b event = function
    | Deity -> Starting
    | Starting -> Nations
    | Nations -> Support
    | Support -> End
    | End -> End

  let next : type a b. a event -> a -> b event =
    fun ev x ->
      apply ev x;
      next_of ev
end
