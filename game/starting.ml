module type S = sig
  val buildings : Build.kind list
  val month : Month.t
  val supply : Defs.supply
  val units : Units.t
end

type t = (module S)

module Apply (S : State.S) = struct
  let value (module Starting : S) =
    S.Build.map (Build.set_ready_ls Starting.buildings);
    S.Month.set Starting.month;
    S.Supply.set Starting.supply;
    S.Units.set Starting.units
end

let building_of = function
  | Leader.Aristocrat -> Build.Stable
  | Leader.Engineer -> Build.Engrs
  | Leader.Merchant -> Build.Trade

let units_of = function
  | Leader.Aristocrat -> Units.(make 10 Cavalry)
  | Leader.Engineer -> Units.(make 1 Ballista)
  | Leader.Merchant -> Units.empty

let to_units ldr mnp =
  units_of ldr |> Units.(add mnp Men)

module Make (S : State.S) = struct
  module Deity = Deity.Roll(S.Dice)
  module Month = Month.Roll(S.Dice)
  let ldr = S.Leader.return Leader.kind_of
  let value = (module struct
    let buildings = Build.([Tavern; building_of ldr])
    let month = Month.random ()
    let res = S.Deity.return Deity.starting
    let supply = Resource.supp_of res
    let units = to_units ldr (Resource.manp_of res)
  end : S)
end
