open Defs

module B = Buildings
module LeaderS = Leader_state
module R = Resource

type enemy = Enemy.party

type t =
  { mutable builds : B.t;
    deity : Deity.t;
    mutable enemies : enemy list;
    mutable leader : LeaderS.t;
    mutable manp : manpower;
    mutable nats : Nation.t list;
    mutable scouting : bool;
    mutable supp : supply;
    mutable turn : turn
  }

module type Init = sig
  val deity : Deity.t
  val leader : Leader.t
end

module type S = sig
  module Cavalry : Value.Num
  val build : Building.t list -> unit
  val built : unit -> Building.t list
  val bld_raze : Building.t -> unit
  val bld_queued : unit -> B.queued list
  val bld_count : Building.t -> int
  val bld_ready : Building.t -> bool
  val bld_manp_cost : unit -> manpower
  val bld_supp_cost : unit -> supply
  val bld_manp : manpower -> unit
  val bld_supp : supply -> unit
  val bld_tick : unit -> unit
  val get_turn : unit -> turn
  val set_turn : turn -> unit
  val add_res : R.t -> unit
  val sub_res : R.t -> unit
  val has_manp : unit -> bool
  val get_manp : unit -> manpower
  val set_manp : manpower -> unit
  val sub_manp : manpower -> unit
  val buy_manp_with : (supply -> manpower * supply) -> unit
  val with_supp : (supply -> 'a) -> 'a
  val add_supp : supply -> unit
  val sub_supp : supply -> unit
  val get_supp : unit -> supply
  val clr_supp : unit -> unit
  val get_deity : unit -> Deity.t
  val with_deity : (Deity.t -> 'a) -> 'a
  val get_nats : unit -> Nation.t list
  val set_nats : Nation.t list -> unit
  val get_ldr : unit -> Leader.t option
  val set_ldr : Leader.t -> unit
  val need_ldr : unit -> bool
  val ldr_tick : unit -> unit
  val ldr_died : unit -> unit
  val is_scouting : unit -> bool
  val set_scouting : bool -> unit
  val get_enemies : unit -> enemy list
  val set_enemies : enemy list -> unit
  val map_enemies : (enemy list -> enemy list) -> unit
  val with_enemies : (enemy list -> 'a) -> 'a
end

module Make (M : Init) : S = struct
  module Cavalry = Value.Num(Value.Zero)

  let t =
    { builds = B.empty;
      deity = M.deity;
      enemies = [];
      leader = LeaderS.make M.leader;
      manp = 0;
      nats = [];
      scouting = false;
      supp = 0;
      turn = 0
    }

  let get_turn () = t.turn
  let set_turn x = t.turn <- x

  let get_supp () = t.supp
  let set_supp x = t.supp <- x
  let clr_supp () = t.supp <- 0
  let add_supp x = t.supp <- t.supp + x
  let sub_supp x = t.supp <- t.supp - x
  let with_supp f = f t.supp

  let get_manp () = t.manp
  let set_manp x = t.manp <- x
  let has_manp () = t.manp > 0
  let add_manp x = t.manp <- t.manp + x
  let sub_manp x = t.manp <- t.manp - x
  let buy_manp_with f =
    let mp, sp = f t.supp in
    add_manp mp;
    sub_supp sp

  let add_res r =
    add_manp (R.manp_of r);
    add_supp (R.supp_of r)

  let sub_res r =
    sub_manp (R.manp_of r);
    sub_supp (R.supp_of r)

  let map_bld f = t.builds <- f t.builds
  let build ls = map_bld (B.build ls)
  let built () = B.built t.builds
  let bld_raze b = map_bld (B.raze b)
  let bld_queued () = B.in_queue t.builds
  let bld_count b = B.count_of b t.builds
  let bld_ready b = B.is_ready b t.builds
  let bld_manp_cost () = B.manp_cost t.builds
  let bld_supp_cost () = B.supp_cost t.builds
  let bld_manp m = map_bld (B.apply_manp t.manp m)
  let bld_supp s =
    let s2, b = B.deduce t.supp s t.builds in
    t.supp <- s2;
    t.builds <- b
  let bld_tick () = map_bld B.tick

  let get_deity () = t.deity
  let with_deity f = f t.deity

  let get_nats () = t.nats
  let set_nats ns = t.nats <- Nation.filter ns

  let get_ldr () = LeaderS.get t.leader
  let set_ldr x = t.leader <- LeaderS.make x
  let need_ldr () = LeaderS.need t.leader
  let ldr_tick () = t.leader <- LeaderS.tick t.leader
  let ldr_died () = t.leader <- LeaderS.dead

  let is_scouting () = t.scouting
  let set_scouting x = t.scouting <- x

  let get_enemies () = t.enemies
  let set_enemies x = t.enemies <- x
  let map_enemies f = set_enemies (f t.enemies)
  let with_enemies f = f t.enemies
end
