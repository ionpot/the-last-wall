module type S = sig
  module Barraging : Value.Bit
  module Build : Value.S with type t = Build.t
  module Cavalry : Value.Num
  module Deity : Value.S with type t = Deity.t
  module Dice : Dice.S
  module Ended : Value.Bit
  module Enemy : Value.S with type t = Enemy.t
  module Leader : Value.S with type t = Leader.t
  module Men : Value.Num
  module Month : Value.S with type t = Month.t
  module Nation : Value.S with type t = Nation.t
  module Scout : Value.Bit
  module Supply : Value.Num
  module Turn : Value.Num
  module Weather : Value.S with type t = Weather.t
  val add_res : Resource.t -> unit
end

module Make (D : Dice.From) : S = struct
  module Barraging = Value.Bit(Value.False)
  module Build = Value.From(Build)
  module Cavalry = Value.Num(Value.Zero)
  module Deity = Value.From(Deity)
  module Dice = Dice.From(D)
  module Ended = Value.Bit(Value.False)
  module Enemy = Value.From(Enemy)
  module Leader = Value.From(Leader)
  module Men = Value.Num(Value.Zero)
  module Month = Value.From(Month)
  module Nation = Value.From(Nation)
  module Scout = Value.Bit(Value.False)
  module Supply = Value.Num(Value.Zero)
  module Turn = Value.Num(Value.Zero)
  module Weather = Value.From(Weather)
  let add_res r =
    Men.add (Resource.manp_of r);
    Supply.add (Resource.supp_of r)
end
