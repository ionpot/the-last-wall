open Game_defs

module type State = Game_state.T

module Outcome = Game_outcome
module Resource = Game_resource
module Support = Game_support

type resource = Resource.t
type support = Support.t

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

module Make(S : State) : T = struct
  let first () = Deity (S.get_deity ())

  let apply = function
    | Starting x -> S.add_res x
    | Support x -> S.add_res (Support.total_of x)
    | Deity x -> S.set_deity x
    | End -> ()
    | Nations x -> S.set_nats x

  let next_of = function
    | Deity _ -> Starting (Outcome.starting (S.get_deity ()))
    | Starting _ -> Nations (S.get_nats ())
    | Nations _ -> Support (Support.of_list (S.get_nats ()))
    | Support _ -> End
    | End -> End

  let next ev =
    apply ev;
    next_of ev
end
