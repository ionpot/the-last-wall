open Defs

module B = Buildings

type t =
  { mutable builds : B.t;
    mutable nats : Nation.t list
  }

module type S = sig
  module Barraging : Value.Bit
  module Cavalry : Value.Num
  module Deity : Value.S with type t = Deity.t
  module Ended : Value.Bit
  module Enemy : Value.S with type t = Enemy.t
  module Leader : Value.S with type t = Leader.t
  module Men : Value.Num
  module Scout : Value.Bit
  module Supply : Value.Num
  module Turn : Value.Num
  val build : Building.t list -> unit
  val bld_raze : Building.t -> unit
  val bld_count : Building.t -> int
  val bld_ready : Building.t -> bool
  val bld_manp_cost : unit -> manpower
  val bld_supp_cost : unit -> supply
  val bld_manp : manpower -> unit
  val bld_supp : supply -> unit
  val with_bld : (B.t -> 'a) -> 'a
  val bld_status : unit -> B.status
  val bld_update : B.status -> unit
  val get_nats : unit -> Nation.t list
  val set_nats : Nation.t list -> unit
end

module Make ( ) : S = struct
  module Barraging = Value.Bit(Value.False)
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
    { builds = B.empty;
      nats = []
    }

  let map_bld f = t.builds <- f t.builds
  let build ls = map_bld (B.build ls)
  let bld_raze b = map_bld (B.raze b)
  let bld_count b = B.count_of b t.builds
  let bld_ready b = B.is_ready b t.builds
  let bld_manp_cost () = B.manp_cost t.builds
  let bld_supp_cost () = B.supp_cost t.builds
  let bld_manp m = map_bld (B.apply_manp (Men.get ()) m)
  let bld_supp s =
    let s2, b = B.deduce (Supply.get ()) s t.builds in
    Supply.set s2;
    t.builds <- b
  let bld_status () = B.to_status t.builds
  let bld_update status = map_bld (B.update status)
  let with_bld f = f t.builds

  let get_nats () = t.nats
  let set_nats ns = t.nats <- Nation.filter ns
end
