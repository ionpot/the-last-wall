module C = Cond
module E = Event

type event =
  | Defeat
  | Scout of C.Scout.t
  | Turn of E.Turn.t

module S : Phase.S with type t = event = struct
  module State = Game.State

  type t = event

  type cond = D | S
  type event = T

  type step = (cond, event) Step.t
  type kind = (cond, event) Step.kind

  let steps = [Step.Do T; Step.CheckIf (S, D)]

  let cond_of : cond -> (module C.S) = function
    | S -> (module C.Scout)
    | D -> (module C.Defeat)

  let t2kind = function
    | Defeat -> Step.Cond D
    | Scout _ -> Step.Cond S
    | Turn _ -> Step.Evt T

  module Apply (S : State.S) = struct
    let value = function
      | Defeat -> let module M = C.Defeat.Apply(S) in M.value ()
      | Scout x -> let module M = C.Scout.Apply(S) in M.value x
      | Turn x -> let module M = E.Turn.Apply(S) in M.value x
  end

  module Make (S : State.S) = struct
    let cond = function
      | S -> let module M = C.Scout.Make(S) in Scout M.value
      | D -> Defeat

    let evt = function
      | T -> let module M = E.Turn.Make(S) in Turn M.value
  end
end
