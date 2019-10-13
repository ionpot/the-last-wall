module type S = sig
  module Arena : Value.Num
  module Ballista : Value.Num
  module Barrage : Value.S with type t = Barrage.t
  module Build : Value.S with type t = Build.t
  module Casualty : Value.S with type t = Units.t
  module Deity : Value.S with type t = Deity.t
  module Dervish : Value.Num
  module Dice : Dice.S
  module Disease : Value.Float
  module Ended : Value.Bit
  module Enemy : Value.S with type t = Units.t
  module Harpy : Value.Float
  module Leader : Value.S with type t = Leader.t
  module Month : Value.S with type t = Month.t
  module Nation : Value.S with type t = Nation.t
  module Scout : Value.Bit
  module Starved : Value.S with type t = Units.t
  module Supply : Value.Num
  module Turn : Value.Num
  module Units : Value.S with type t = Units.t
  module Weather : Value.S with type t = Weather.t
end

module Make (D : Dice.From) : S = struct
  module Arena = Value.Num(Value.Zero)
  module Ballista = Value.Num(Value.Zero)
  module Barrage = Value.From(Barrage)
  module Build = Value.From(Build)
  module Casualty = Value.From(Units)
  module Deity = Value.From(Deity)
  module Dervish = Value.Num(Value.Zero)
  module Dice = Dice.From(D)
  module Disease = Value.Float(Value.ZeroF)
  module Ended = Value.Bit(Value.False)
  module Enemy = Value.From(Units)
  module Harpy = Value.Float(Value.ZeroF)
  module Leader = Value.From(Leader)
  module Month = Value.From(Month)
  module Nation = Value.From(Nation)
  module Scout = Value.Bit(Value.False)
  module Starved = Value.From(Units)
  module Supply = Value.Num(Value.Zero)
  module Turn = Value.Num(Value.Zero)
  module Units = Value.From(Units)
  module Weather = Value.From(Weather)
end
