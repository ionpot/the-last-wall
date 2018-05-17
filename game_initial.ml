open Game_defs

module type State = Game_state.T

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

module Make(M : State) : T = struct
  let first () = Deity (M.get_deity ())

  let apply = function
    | Starting x -> M.add_res x
    | Support x -> M.add_res (S.total_of x)
    | Deity x -> M.set_deity x
    | End -> ()
    | Nations x -> M.set_nats x

  let next_of = function
    | Deity _ -> Starting (O.starting (M.get_deity ()))
    | Starting _ -> Nations (M.get_nats ())
    | Nations _ -> Support (S.of_list (M.get_nats ()))
    | Support _ -> End
    | End -> End

  let next ev =
    apply ev;
    next_of ev
end
