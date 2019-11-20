type action = New | Promote | Train
type pool =
  | Add of Pool.kind
  | Exclude of Pool.kind
  | From of Pool.kind
  | Set of Pool.kind

module type Cap = State.S -> sig
  val value : Defs.count option
end

module type Type = sig
  val action : action
  val kind : Units.kind
  val pool : pool option
  module Cap : Cap
end

module Attr = Units.Attr
module Base = Units.Base
module Promote = Units.Promote

let bld_of k =
  if k = Units.Berserker then Some Build.Arena
  else if Attr.is_cavalry k then Some Build.Stable
  else if Attr.is_holy k then Some Build.Temple
  else if Attr.is_siege k then Some Build.Engrs
  else None

let attr_of b k =
  match b, bld_of k with
  | Some a, Some b -> a = b
  | _ -> false

let translate n k_in k_out =
  Power.translate k_in k_out n Power.empty

module Build (S : State.S) = struct
  let cap_of b =
    S.Build.return (Build.cap_of b)

  let temple_cap () =
    cap_of Build.Temple + cap_of Build.Guesthouse

  let bld_cap = function
    | Some Build.Temple -> Some (temple_cap ())
    | Some b -> Some (cap_of b)
    | None -> None

  let vacancy kind =
    let bld = bld_of kind in
    let attr = attr_of bld in
    let count =
      S.Units.return (Units.filter_count attr)
      + S.Training.return (Units.filter_count attr)
    in
    match bld_cap bld with
    | Some x -> Some (Number.sub x count)
    | None -> None
end

module Check (S : State.S) = struct
  module Check = Support.Check(S)

  let ldr_is kind =
    S.Leader.check (Leader.is_living kind)

  let traded = Check.has_traded
end

module Pool (S : State.S) = struct
  let map = S.Pool.map

  let get pk =
    S.Pool.return (Pool.get pk)

  let apply' n kind = function
    | Add pk -> Pool.add pk n |> map
    | Exclude _ -> ()
    | From pk ->
        let k = Promote.needs kind in
        let p = get pk in
        let u = S.Units.return (Units.count k) in
        let n' = min p u - translate n kind k in
        Pool.set pk n' |> map
    | Set pk -> Pool.set pk n |> map

  let apply n kind = function
    | Some ptype -> apply' n kind ptype
    | None -> ()

  let to_units kind ptype =
    let units = S.Units.get () in
    match ptype with
    | Exclude pk -> Units.sub (get pk) kind units
    | From pk ->
        let k = Promote.needs kind in
        let n = Units.count k units |> min (get pk) in
        Units.make n k
    | Add _
    | Set _ -> units

  let units kind = function
    | Some p -> to_units kind p
    | None -> S.Units.get ()
end

module Supply (S : State.S) = struct
  module Check = Check(S)

  let ldr_is = Check.ldr_is
  let traded = Check.traded

  let bonus_for kind = 0.
    |> Float.add_if (ldr_is Leader.Aristocrat && Attr.is_cavalry kind) 0.2
    |> Float.add_if (ldr_is Leader.Merchant && kind = Units.Merc) 0.1
    |> Float.add_if (traded Nation.Clan && Attr.is_siege kind) 0.2

  let avlb kind =
    Number.increase_by (bonus_for kind)
    |> S.Supply.return

  let cost n kind =
    (n * Base.supply_cost kind)
    |> Number.reduce_by (bonus_for kind)

  let limit kind cap =
    let cost = Base.supply_cost kind in
    if cost > 0
    then min cap (avlb kind / cost)
    else cap

  let sub n kind =
    S.Supply.sub (cost n kind)
end

module Train (S : State.S) = struct
  module Check = Check(S)

  let is_fast kind =
    if Attr.is_siege kind
    then Check.ldr_is Engineer
    else false

  let apply n kind =
    let pop = Units.pop kind in
    let trained, rest = S.Training.return pop in
    let a, b = if is_fast kind then 0, n else n, 0 in
    S.Training.set (Units.add a kind rest);
    Units.add b kind trained
    |> Units.combine
    |> S.Units.map
end

module Event (T : Type) = struct
  type t = Defs.count
  module Apply (S : State.S) = struct
    module Pool = Pool(S)
    module Supply = Supply(S)
    module Train = Train(S)
    let value n =
      Pool.apply n T.kind T.pool;
      Supply.sub n T.kind;
      Promote.cost n T.kind |> Units.reduce |> S.Units.map;
      if T.action = Train
      then Train.apply n T.kind
      else S.Units.map (Units.add n T.kind)
  end
  module Make (S : State.S) = struct
    module Build = Build(S)
    module Cap = T.Cap(S)
    module Pool = Pool(S)
    module Supply = Supply(S)
    let units = Pool.units T.kind T.pool
    let count =
      match T.action with
      | New | Train ->
          (Build.vacancy T.kind
          |> Number.opt2_min Cap.value
          |> Promote.affordable T.kind) units
      | Promote ->
          Promote.max T.kind units
          |> Number.opt_min Cap.value
    let value = Supply.limit T.kind count
  end
end

module NoCap (_ : State.S) = struct
  let value = None
end
