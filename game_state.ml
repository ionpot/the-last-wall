open Game_defs

module R = Game_resource

type resource = R.t
type turn = int
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
  val get_deity : unit -> deity
  val set_deity : deity -> unit
  val get_nats : unit -> nation list
  val set_nats : nation list -> unit
  val get_ldr : unit -> leader
  val set_ldr : leader -> unit
  val defeat : unit -> bool
end

module Make( ) : T = struct
  open R

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

  let get_deity () = t.deity
  let set_deity d = t.deity <- d

  let get_nats () = t.nats
  let set_nats ns = t.nats <- pick_nats max_nats ns

  let get_ldr () = t.leader
  let set_ldr x = t.leader <- x

  let defeat () = (manp t.res) <= 0
end
