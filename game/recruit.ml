type action = Add | Promote | Train
type pool =
  | Exclude of Pool.kind
  | From of Pool.kind * Units.kind
  | To of Pool.kind

module type Cap = State.S -> sig
  val value : Defs.count option
end

module type Type = sig
  val action : action
  val kind : Units.kind
  val pool : pool option
  module Cap : Cap
end

module Event (T : Type) = struct
  type t = Defs.count
  module Apply (_ : State.S) = struct let value _ = () end
  module Make (_ : State.S) = struct let value = 0 end
end

module NoCap (_ : State.S) = struct let value = None end

module Attr = Units.Attr

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

module With (S : State.S) = struct
  module Base = Units.Base
  module Check = Support.Check(S)

  let bld_cap_of b =
    S.Build.return (Build.cap_of b)

  let temple_cap () =
    bld_cap_of Build.Temple + bld_cap_of Build.Guesthouse

  let bld_cap = function
    | Some Build.Temple -> temple_cap ()
    | Some b -> bld_cap_of b
    | None -> 0

  let ldr_is kind =
    S.Leader.check (Leader.is_living kind)

  let is_fast kind =
    if Attr.is_siege kind then ldr_is Engineer else false

  let traded = Check.has_traded

  let bonus_for kind = 0.
    |> Float.add_if (ldr_is Leader.Aristocrat && Attr.is_cavalry kind) 0.2
    |> Float.add_if (ldr_is Leader.Merchant && kind = Units.Merc) 0.1
    |> Float.add_if (traded Nation.Clan && Attr.is_siege kind) 0.2

  let avlb_sup kind =
    Number.increase_by (bonus_for kind)
    |> S.Supply.return

  let cost_of kind n =
    (Base.supply_cost kind * n)
    |> Number.reduce_by (bonus_for kind)

  module Missing = struct
    let arena () =
      Units.(count Berserker)
      |> S.Units.return
      |> Number.sub (bld_cap_of Build.Arena)

    let stable () =
      Units.filter_count Attr.is_cavalry
      |> S.Units.return
      |> Number.sub (bld_cap_of Build.Stable)

    let temple () =
      Units.filter_count Attr.is_holy
      |> S.Units.return
      |> Number.sub (temple_cap ())
  end

  let supply_limit kind cap =
    let cost = Base.supply_cost kind in
    if cost > 0 then
      let supp = avlb_sup kind in
      min cap (supp / cost)
    else cap

  let affordable kind cap =
    S.Units.get () |> Units.affordable kind cap |> supply_limit kind

  let promotable kind =
    S.Units.get () |> Units.promotable kind |> supply_limit kind

  let vacancy kind =
    let bld = bld_of kind in
    let attr = attr_of bld in
    bld_cap bld
    - S.Units.return (Units.filter_count attr)
    - S.Training.return (Units.filter_count attr)

  let trainable kind =
    let cap = vacancy kind in
    S.Units.return (Units.affordable kind cap)
    |> supply_limit kind

  let sub_cost kind n =
    S.Supply.sub (cost_of kind n);
    S.Units.map Units.(cost n kind |> reduce)

  let promote kind n =
    sub_cost kind n;
    S.Units.map Units.(add n kind)

  let train kind n =
    sub_cost kind n;
    let pool, rest = S.Training.return (Units.pop kind) in
    let a, b = if is_fast kind then 0, n else n, 0 in
    let n' = Units.count_all pool + b in
    S.Units.map (Units.add n' kind);
    S.Training.set (Units.add a kind rest)
end
