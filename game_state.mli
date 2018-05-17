open Game_defs

type resource = Game_resource.t
type turn = int

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
module Make( ) : T
