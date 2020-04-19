module type S = sig
  val buildings : Build.kind list
  val month : Month.t
  val resource : Resource.t
  val units : Units.t
end

type t = (module S)

let apply (module Starting : S) state =
  state
  |> State.build_map (Build.set_ready_ls Starting.buildings)
  |> State.month_set Starting.month
  |> State.resource_set Starting.resource
  |> State.units_set Starting.units

let building_of = function
  | Leader.Aristocrat -> Build.Stable
  | Leader.Engineer -> Build.Engrs
  | Leader.Merchant -> Build.Trade

let units_of = function
  | Leader.Aristocrat -> Units.(make 10 Cavalry)
  | Leader.Engineer -> Units.(make 1 Ballista)
  | Leader.Merchant -> Units.empty

let make state =
  let ldr = Leader.kind_of (State.leader state) in
  (module struct
    let buildings = Build.([Tavern; building_of ldr])
    let month = Month.Roll.random ()
    let resource = Deity.Roll.starting (State.deity state)
    let units = units_of ldr
  end : S)
