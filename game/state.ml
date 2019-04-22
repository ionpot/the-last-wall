module type S = sig
  module Barraging : Value.Bit
  module Build : Value.S with type t = Build.t
  module Deity : Value.S with type t = Deity.t
  module Dice : Dice.S
  module Ended : Value.Bit
  module Enemy : Value.S with type t = Units.t
  module Leader : Value.S with type t = Leader.t
  module Month : Value.S with type t = Month.t
  module Nation : Value.S with type t = Nation.t
  module Scout : Value.Bit
  module Supply : Value.Num
  module Turn : Value.Num
  module Units : Value.S with type t = Units.t
  module Weather : Value.S with type t = Weather.t
end

module Make (D : Dice.From) : S = struct
  module Barraging = Value.Bit(Value.False)
  module Build = Value.From(Build)
  module Deity = Value.From(Deity)
  module Dice = Dice.From(D)
  module Ended = Value.Bit(Value.False)
  module Enemy = Value.From(Units)
  module Leader = Value.From(Leader)
  module Month = Value.From(Month)
  module Nation = Value.From(Nation)
  module Scout = Value.Bit(Value.False)
  module Supply = Value.Num(Value.Zero)
  module Turn = Value.Num(Value.Zero)
  module Units = Value.From(Units)
  module Weather = Value.From(Weather)
end
