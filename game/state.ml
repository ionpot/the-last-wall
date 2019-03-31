open Defs

module B = Buildings
module Ldr = Leader
module R = Resource

type enemy = Enemy.party

type t =
  { mutable builds : B.t;
    mutable enemies : enemy list;
    mutable manp : manpower;
    mutable nats : Nation.t list;
    mutable scouting : bool;
    mutable supp : supply
  }

module type S = sig
  module Barraging : Value.Bit
  module Cavalry : Value.Num
  module Deity : Value.S with type t = Deity.t
  module Ended : Value.Bit
  module Leader : Value.S with type t = Ldr.t
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
  val add_res : R.t -> unit
  val sub_res : R.t -> unit
  val has_manp : unit -> bool
  val no_manp : unit -> bool
  val get_manp : unit -> manpower
  val set_manp : manpower -> unit
  val sub_manp : manpower -> unit
  val with_supp : (supply -> 'a) -> 'a
  val has_supp : unit -> bool
  val add_supp : supply -> unit
  val sub_supp : supply -> unit
  val get_supp : unit -> supply
  val clr_supp : unit -> unit
  val supp2manp : supply -> unit
  val get_nats : unit -> Nation.t list
  val set_nats : Nation.t list -> unit
  val is_scouting : unit -> bool
  val set_scouting : bool -> unit
  val get_enemies : unit -> enemy list
  val set_enemies : enemy list -> unit
  val map_enemies : (enemy list -> enemy list) -> unit
  val with_enemies : (enemy list -> 'a) -> 'a
end

module Make ( ) : S = struct
  module Barraging = Value.Bit(Value.False)
  module Cavalry = Value.Num(Value.Zero)
  module Deity = Value.From(Deity)
  module Ended = Value.Bit(Value.False)
  module Leader = Value.From(Ldr)
  module Turn = Value.Num(Value.Zero)

  let t =
    { builds = B.empty;
      enemies = [];
      manp = 0;
      nats = [];
      scouting = false;
      supp = 0
    }

  let get_supp () = t.supp
  let set_supp x = t.supp <- x
  let clr_supp () = t.supp <- 0
  let has_supp () = t.supp > 0
  let add_supp x = t.supp <- t.supp + x
  let sub_supp x = t.supp <- t.supp - x
  let with_supp f = f t.supp

  let get_manp () = t.manp
  let set_manp x = t.manp <- x
  let has_manp () = t.manp > 0
  let no_manp () = t.manp <= 0
  let add_manp x = t.manp <- t.manp + x
  let sub_manp x = t.manp <- t.manp - x

  let supp2manp sup =
    let s = min sup t.supp in
    add_manp s;
    sub_supp s

  let add_res r =
    add_manp (R.manp_of r);
    add_supp (R.supp_of r)

  let sub_res r =
    sub_manp (R.manp_of r);
    sub_supp (R.supp_of r)

  let map_bld f = t.builds <- f t.builds
  let build ls = map_bld (B.build ls)
  let bld_raze b = map_bld (B.raze b)
  let bld_count b = B.count_of b t.builds
  let bld_ready b = B.is_ready b t.builds
  let bld_manp_cost () = B.manp_cost t.builds
  let bld_supp_cost () = B.supp_cost t.builds
  let bld_manp m = map_bld (B.apply_manp t.manp m)
  let bld_supp s =
    let s2, b = B.deduce t.supp s t.builds in
    t.supp <- s2;
    t.builds <- b
  let bld_status () = B.to_status t.builds
  let bld_update status = map_bld (B.update status)
  let with_bld f = f t.builds

  let get_nats () = t.nats
  let set_nats ns = t.nats <- Nation.filter ns

  let is_scouting () = t.scouting
  let set_scouting x = t.scouting <- x

  let get_enemies () = t.enemies
  let set_enemies x = t.enemies <- x
  let map_enemies f = set_enemies (f t.enemies)
  let with_enemies f = f t.enemies
end
