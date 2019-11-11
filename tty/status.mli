module type S = sig
  val berserker : unit -> unit
  val cavalry : Game.Defs.count -> unit
  val dervish : unit -> unit
  val enemies : unit -> unit
  val facilities : Game.Direct.Facilities.t -> unit
  val leader : unit -> unit
  val new_leader : Game.Input.LeaderNew.t -> unit
  val ranger : unit -> unit
  val res : unit -> unit
  val templar : unit -> unit
  val units : unit -> unit
end

module With : Game.State.S -> S
