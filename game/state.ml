open Defs

module LeaderS = Leader_state
module R = Resource

type enemy = Enemy.party

type t =
  { mutable builds : Building.state;
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
  val get_bld : unit -> Building.state
  val build : Building.t list -> unit
  val bld_report : unit -> Building.report
  val bld_supp : unit -> unit
  val bld_tick : unit -> unit
  val get_turn : unit -> turn
  val set_turn : turn -> unit
  val get_res : unit -> R.t
  val set_res : R.t -> unit
  val add_res : R.t -> unit
  val sub_res : R.t -> unit
  val has_manp : unit -> bool
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
end

module Make (M : Init) : S = struct
  let t =
    { builds = Building.initial;
      deity = M.deity;
      enemies = [];
      leader = LeaderS.make M.leader;
      nats = [];
      res = R.empty;
      scouting = false;
      turn = 0
    }

  let get_bld () = t.builds
  let build ls = t.builds <- Building.build ls t.builds
  let bld_ready b = Building.is_ready b t.builds
  let bld_report () = Building.report_of t.builds
  let bld_apply f =
    let r, b = f t.res t.builds in
    t.res <- r; t.builds <- b
  let bld_supp () = bld_apply Building.draw_supp
  let bld_tick () = bld_apply Building.tick

  let get_turn () = t.turn
  let set_turn x = t.turn <- x

  let get_res () = t.res
  let set_res r = t.res <- r
  let add_res r = t.res <- R.(t.res ++ r)
  let sub_res r = t.res <- R.(t.res -- r)
  let has_manp () = R.has_manp t.res
  let clr_supp () = t.res <- R.clr_supp t.res

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
end
