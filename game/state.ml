type t =
  { mutable nats : Nation.t list
  }

module type S = sig
  module Barraging : Value.Bit
  module Build : Value.S with type t = Build.t
  module Cavalry : Value.Num
  module Deity : Value.S with type t = Deity.t
  module Ended : Value.Bit
  module Enemy : Value.S with type t = Enemy.t
  module Leader : Value.S with type t = Leader.t
  module Men : Value.Num
  module Scout : Value.Bit
  module Supply : Value.Num
  module Turn : Value.Num
  val get_nats : unit -> Nation.t list
  val set_nats : Nation.t list -> unit
end

module Make ( ) : S = struct
  module Barraging = Value.Bit(Value.False)
  module Build = Value.From(Build)
  module Cavalry = Value.Num(Value.Zero)
  module Deity = Value.From(Deity)
  module Ended = Value.Bit(Value.False)
  module Enemy = Value.From(Enemy)
  module Leader = Value.From(Leader)
  module Men = Value.Num(Value.Zero)
  module Scout = Value.Bit(Value.False)
  module Supply = Value.Num(Value.Zero)
  module Turn = Value.Num(Value.Zero)

  let t =
    { nats = []
    }

  let get_nats () = t.nats
  let set_nats ns = t.nats <- Nation.filter ns
end
