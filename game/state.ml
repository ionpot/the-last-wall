type t = unit
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
