open Defs

module R = Resource

type enemy = Enemy.party

type t =
  { mutable deity : Deity.t;
    mutable enemies : enemy list;
    mutable leader : Leader.state;
    mutable nats : Nation.t list;
    mutable res : R.t;
    mutable scouting : bool;
    mutable turn : turn
  }

module type S = sig
  val get_turn : unit -> turn
  val set_turn : turn -> unit
  val get_res : unit -> R.t
  val set_res : R.t -> unit
  val add_res : R.t -> unit
  val sub_res : R.t -> unit
  val has_manp : unit -> bool
  val clr_supp : unit -> unit
  val get_deity : unit -> Deity.t
  val set_deity : Deity.t -> unit
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

module Make( ) : S = struct
  let max_nats = 3

  let t =
    { deity = Deity.default;
      enemies = [];
      leader = Leader.empty;
      nats = [];
      res = R.empty;
      scouting = false;
      turn = 0
    }

  let get_turn () = t.turn
  let set_turn x = t.turn <- x

  let get_res () = t.res
  let set_res r = t.res <- r
  let add_res r = t.res <- R.(t.res ++ r)
  let sub_res r = t.res <- R.(t.res -- r)
  let has_manp () = R.has_manp t.res
  let clr_supp () = t.res <- R.clr_supp t.res

  let get_deity () = t.deity
  let set_deity d = t.deity <- d

  let get_nats () = t.nats
  let set_nats ns = t.nats <- Nation.pickN max_nats ns

  let get_ldr () = Leader.of_state t.leader
  let set_ldr x = t.leader <- Leader.state_of x
  let need_ldr () = Leader.need t.leader
  let ldr_tick () = t.leader <- Leader.tick t.leader
  let ldr_died () = t.leader <- Leader.dead

  let is_scouting () = t.scouting
  let set_scouting x = t.scouting <- x

  let get_enemies () = t.enemies
  let set_enemies x = t.enemies <- x
end
