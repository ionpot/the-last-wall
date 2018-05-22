open Game_defs

type resource = Game_resource.t

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
  val is_scouting : unit -> bool
  val set_scouting : bool -> unit
end

module Make( ) : T
