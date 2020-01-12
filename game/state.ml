module type S = sig
  module Barrage : Value.S with type t = Barrage.t
  module Bonus : Value.S with type t = Bonus.t
  module Build : Value.S with type t = Build.t
  module Casualty : Value.S with type t = Units.t
  module Deity : Value.S with type t = Deity.t
  module Dice : Dice.S
  module Ended : Value.Bit
  module Enemy : Value.S with type t = Units.t
  module Feared : Value.S with type t = Units.t
  module Harpy : Value.Float
  module Leader : Value.S with type t = Leader.t
  module Mishap : Value.S with type t = Mishap.t
  module Month : Value.S with type t = Month.t
  module Nation : Value.S with type t = Nation.t
  module Pool : Value.S with type t = Pool.t
  module Research : Value.S with type t = Research.t
  module Scout : Value.Bit
  module Starved : Value.S with type t = Units.t
  module Supply : Value.Num
  module Training : Value.S with type t = Units.t
  module Turn : Value.Num
  module Units : Value.S with type t = Units.t
  module Weather : Value.S with type t = Weather.t
end

module Make (D : Dice.From) : S = struct
  module Arena = Value.Num(Value.Zero)
  module Barrage = Value.From(Barrage)
  module Bonus = Value.From(Bonus)
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
