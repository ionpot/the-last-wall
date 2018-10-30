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
    mutable nats : Nation.t list;
    mutable res : R.t;
    mutable scouting : bool;
    mutable turn : turn
  }

module type Init = sig
  val deity : Deity.t
  val leader : Leader.t
end

module type S = sig
  val build : Building.t list -> unit
  val built : unit -> Building.t list
  val bld_queued : unit -> B.queued list
  val bld_count : Building.t -> int
  val bld_ready : Building.t -> bool
  val bld_manp : unit -> unit
  val bld_supp : unit -> unit
  val bld_tick : unit -> unit
  val get_turn : unit -> turn
  val set_turn : turn -> unit
  val get_res : unit -> R.t
  val map_res : (R.t -> R.t) -> unit
  val set_res : R.t -> unit
  val add_res : R.t -> unit
  val sub_res : R.t -> unit
  val with_res : (R.t -> 'a) -> 'a
  val has_manp : unit -> bool
  val sub_manp : manpower -> unit
  val buy_manp_with : (supply -> manpower * supply) -> unit
  val with_supp : (supply -> 'a) -> 'a
  val sub_supp : supply -> unit
  val get_supp : unit -> supply
  val clr_supp : unit -> unit
  val get_deity : unit -> Deity.t
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
  let t =
    { builds = B.make ();
      deity = M.deity;
      enemies = [];
      leader = LeaderS.make M.leader;
      nats = [];
      res = R.empty;
      scouting = false;
      turn = 0
    }

  let get_turn () = t.turn
  let set_turn x = t.turn <- x

  let get_res () = t.res
  let set_res r = t.res <- r
  let map_res f = set_res (f t.res)
  let add_res r = t.res <- R.(t.res ++ r)
  let sub_res r = t.res <- R.(t.res -- r)
  let with_res f = f t.res

  let clr_supp () = t.res <- R.clr_supp t.res
  let get_supp () = R.supp_of t.res
  let set_supp x = t.res <- R.set_supp t.res x
  let sub_supp x = sub_res (R.of_supp x)
  let with_supp f = f (get_supp ())

  let get_manp () = R.manp_of t.res
  let has_manp () = R.has_manp t.res
  let add_manp x = add_res (R.of_manp x)
  let sub_manp x = sub_res (R.of_manp x)
  let set_manp x = t.res <- R.set_manp t.res x
  let buy_manp_with f =
    let mp, sp = f (get_supp ()) in
    add_manp mp;
    sub_supp sp

  let map_bld f = t.builds <- f t.builds
  let build ls = map_bld (B.build ls)
  let built () = B.built t.builds
  let bld_queued () = B.in_queue t.builds
  let bld_count b = B.count_of b t.builds
  let bld_ready b = B.is_ready b t.builds
  let bld_apply (get, set) f =
    let r, b = f (get ()) t.builds in
    set r; t.builds <- b
  let bld_manp () = bld_apply (get_manp, set_manp) B.add_manp
  let bld_supp () = bld_apply (get_supp, set_supp) B.add_supp
  let bld_tick () = map_bld B.tick

  let get_deity () = t.deity

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
