open Defs

type deity = Deity.t
type enemy = Enemy.party
type leader = Leader.t
type nation = Nation.t
type resource = Resource.t

type t =
  { mutable deity : deity;
    mutable leader : leader option;
    mutable nats : nation list;
    mutable res : resource;
    mutable scouting : bool;
    mutable seen : enemy list;
    mutable arrived : enemy list
  }

module type T = sig
  val get_turn : unit -> turn
  val inc_turn : unit -> unit
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
  val has_ldr : unit -> bool
  val clr_ldr : unit -> unit
  val get_ldr : unit -> leader option
  val set_ldr : leader -> unit
  val ldr_won : unit -> unit
  val is_scouting : unit -> bool
  val set_scouting : bool -> unit
  val set_seen : enemy list -> unit
  val get_seen : unit -> enemy list
  val get_arrived : unit -> enemy list
  val move_enemies : enemy list -> unit
end

module Make( ) : T = struct
  open Resource

  let max_nats = 3

  let turn = ref 0

  let t =
    { deity = Deity.None;
      leader = None;
      nats = [];
      res = make Empty;
      scouting = false;
      seen = [];
      arrived = []
    }

  let get_turn () = !turn
  let inc_turn () = incr turn

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
  let missing_supp () =
    let x = supp t.res in
    if x < 0 then abs x else 0

  let get_deity () = t.deity
  let set_deity d = t.deity <- d

  let get_nats () = t.nats
  let set_nats ns = t.nats <- Nation.pickN max_nats ns

  let get_ldr () = t.leader
  let set_ldr x = t.leader <- Some x
  let clr_ldr x = t.leader <- None
  let has_ldr () =
    match t.leader with
    | Some _ -> true
    | None -> false
  let ldr_won () =
    match t.leader with
    | Some x -> Leader.won x
    | None -> ()

  let is_scouting () = t.scouting
  let set_scouting x = t.scouting <- x

  let set_seen x = t.seen <- x
  let get_seen () = t.seen
  let get_arrived () = t.arrived
  let move_enemies seen =
    t.arrived <- t.seen;
    t.seen <- seen
end
