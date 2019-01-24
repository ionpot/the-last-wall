module Of (Ph : Phase.S) = struct
  module Apply = Ph.Apply

  module Make (S : Game.State.S) = struct
    module Make = Ph.Make(S)

    let is_ok e =
      let module Cond = (val Ph.cond_of e : Cond.S) in
      let module M = Cond.Check(S) in
      M.value

    let value = function
      | Step.Cond e -> if is_ok e then Some (Make.cond e) else None
      | Step.Evt e -> Some (Make.evt e)
  end

  type t = This of Ph.kind | Next | Nope

  open Step

  let check step evt =
    match step, evt with
    | Check a, Cond e -> if a = e then Next else Nope
    | CheckIf (a, b), Cond e ->
        if a = e then This (Cond b)
        else if b = e then Next
        else Nope
    | Do a, Evt e -> if a = e then Next else Nope
    | _ -> Nope

  let kind_of = function
    | Check a -> Cond a
    | CheckIf (a, _) -> Cond a
    | Do a -> Evt a

  let first = List.hd Ph.steps

  let next_of evt =
    let rec next eq = function
      | [] -> None
      | step :: xs ->
          if eq then Some (kind_of step)
          else match check step evt with
          | This x -> Some x
          | Next -> next true xs
          | Nope -> next false xs
    in
    next false Ph.steps
end
