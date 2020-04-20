type t =
  { build : Build.t
  ; deity : Deity.t
  ; leader : Leader.t
  ; manpower : Defs.manpower
  ; mishap : Mishap.t
  ; month : Month.t
  ; nation : Nation.t
  ; pool : Pool.t
  ; supply : Defs.supply
  ; turn : Defs.turn
  ; units : Units.t
  }

let empty =
  { build = Build.empty
  ; deity = Deity.empty
  ; leader = Leader.empty
  ; manpower = 0
  ; mishap = Mishap.empty
  ; month = Month.empty
  ; nation = Nation.empty
  ; pool = Pool.empty
  ; supply = 0
  ; turn = 0
  ; units = Units.empty
  }

let build t = t.build
let build_map f t = { t with build = f t.build }

let deity t = t.deity
let deity_set deity t = { t with deity }

let leader t = t.leader
let leader_set leader t = { t with leader }

let mishap t = t.mishap

let month_set month t = { t with month }

let nation t = t.nation
let nation_map f t = { t with nation = f t.nation }

let pool_map f t = { t with pool = f t.pool }

let resource t = Resource.make ~mnp:t.manpower ~sup:t.supply ()
let resource_set res t =
  { t with manpower = Resource.mnp res
  ; supply = Resource.sup res
  }
let resource_map f t = resource_set (resource t |> f) t

let turn t = t.turn

let units_set units t = { t with units }

(*
module Make (D : Dice.From) : S = struct
  module Arena = Value.Num(Value.Zero)
  module Barrage = Value.From(Barrage)
  module Build = Value.From(Build)
  module Casualty = Value.From(Units)
  module Deity = Value.From(Deity)
  module Dervish = Value.Num(Value.Zero)
  module Dice = Dice.From(D)
  module Ended = Value.Bit(Value.False)
  module Enemy = Value.From(Units)
  module Feared = Value.From(Units)
  module Harpy = Value.Float(Value.ZeroF)
  module Leader = Value.From(Leader)
  module Mishap = Value.From(Mishap)
  module Month = Value.From(Month)
  module Nation = Value.From(Nation)
  module Pool = Value.From(Pool)
  module Research = Value.From(Research)
  module Scout = Value.Bit(Value.False)
  module Starved = Value.From(Units)
  module Supply = Value.Num(Value.Zero)
  module Training = Value.From(Units)
  module Turn = Value.Num(Value.Zero)
  module Units = Value.From(Units)
  module Weather = Value.From(Weather)
end
*)

(* move to state.ml
module AddRes (S : State.S) = struct
  let value res =
    S.Supply.add (Resource.sup res);
    S.Units.map Units.(add (Resource.mnp res) Men)
end

module LdrDied (S : State.S) = struct
  let value respawn =
    S.Leader.map (S.Turn.return Leader.died respawn);
    S.Build.map (S.Leader.return Build.died)
end
*)
