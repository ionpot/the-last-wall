open Game_defs

module Resource = Game_resource

type resource = Resource.t

type t =
  { mutable deity : deity;
    mutable leader : leader;
    mutable nats : nation list;
    mutable res : resource
  }

let pick_nats max ns =
  let f n = List.mem n ns in
  nation_list
  |> List.filter f
  |> Listx.pick_first max

module type T = sig
  val get_turn : unit -> turn
  val inc_turn : unit -> unit
  val get_res : unit -> resource
  val add_res : resource -> unit
  val sub_res : resource -> unit
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
end

module Make( ) : T = struct
  open Resource

  let max_nats = 3

  let turn = ref 0

  let t =
    { deity = NoDeity;
      leader = Alive;
      nats = [];
      res = make Empty
    }

  let get_turn () = !turn
  let inc_turn () = incr turn

  let get_res () = t.res
  let add_res r = t.res <- t.res ++ r
  let sub_res r = t.res <- t.res -- r

  let add_manp m = t.res <+ Manpwr m
  let sub_manp m = t.res <~ Manpwr m
  let no_manp () = manp t.res <= 0

  let add_supp m = t.res <+ Supply m
  let sub_supp m = t.res <~ Supply m
  let clr_supp () = t.res <- make (Manpwr (manp t.res))
  let missing_supp () =
    let x = supp t.res in
    if x < 0 then abs x else 0

  let get_deity () = t.deity
  let set_deity d = t.deity <- d

  let get_nats () = t.nats
  let set_nats ns = t.nats <- pick_nats max_nats ns

  let get_ldr () = t.leader
  let set_ldr x = t.leader <- x

  let defeat () = (manp t.res) <= 0
end
