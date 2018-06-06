open Defs

type deity = Deity.t
type enemy = Enemy.party
type leader = Leader.t
type nation = Nation.t
type resource = Resource.t

type t =
  { mutable deity : deity;
    mutable enemies : enemy list;
    mutable leader : leader;
    mutable nats : nation list;
    mutable res : resource;
    mutable scouting : bool;
    mutable turn : turn
  }

module type T = sig
  val get_turn : unit -> turn
  val set_turn : turn -> unit
  val get_res : unit -> resource
  val add_res : resource -> unit
  val sub_res : resource -> unit
  val get_manp : unit -> manpower
  val add_manp : manpower -> unit
  val sub_manp : manpower -> unit
  val no_manp : unit -> bool
  val add_supp : supply -> unit
  val sub_supp : supply -> unit
  val clr_supp : unit -> unit
  val missing_supp : unit -> supply
  val get_deity : unit -> deity
  val set_deity : deity -> unit
  val get_nats : unit -> nation list
  val set_nats : nation list -> unit
  val get_ldr : unit -> leader
  val set_ldr : leader -> unit
  val ldr_alive : unit -> bool
  val ldr_won : unit -> unit
  val ldr_died : unit -> unit
  val ldr_died_at : unit -> turn
  val is_scouting : unit -> bool
  val set_scouting : bool -> unit
  val get_enemies : unit -> enemy list
  val set_enemies : enemy list -> unit
end

module Make( ) : T = struct
  open Resource

  let max_nats = 3

  let t =
    { deity = Deity.default;
      enemies = [];
      leader = Leader.make_empty ();
      nats = [];
      res = make Empty;
      scouting = false;
      turn = 0
    }

  let get_turn () = t.turn
  let set_turn x = t.turn <- x

  let get_res () = t.res
  let add_res r = t.res <- t.res ++ r
  let sub_res r = t.res <- t.res -- r

  let get_manp () = manp t.res
  let add_manp m = t.res <- t.res <+ Manpwr m
  let sub_manp m = t.res <- t.res <~ Manpwr m
  let no_manp () = manp t.res <= 0

  let add_supp m = t.res <- t.res <+ Supply m
  let sub_supp m = t.res <- t.res <~ Supply m
  let clr_supp () = t.res <- make (Manpwr (manp t.res))
  let missing_supp () = supp_missing t.res

  let get_deity () = t.deity
  let set_deity d = t.deity <- d

  let get_nats () = t.nats
  let set_nats ns = t.nats <- Nation.pickN max_nats ns

  let get_ldr () = t.leader
  let set_ldr x = t.leader <- x
  let ldr_alive () = Leader.alive t.leader
  let ldr_won () = Leader.won t.leader
  let ldr_died () = Leader.died t.leader t.turn
  let ldr_died_at () = Leader.died_at t.leader

  let is_scouting () = t.scouting
  let set_scouting x = t.scouting <- x

  let get_enemies () = t.enemies
  let set_enemies x = t.enemies <- x
end
