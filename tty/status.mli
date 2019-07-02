module type S = sig
  val cavalry : Game.Defs.count -> unit
  val dervish : unit -> unit
  val enemies : unit -> unit
  val enemies_remaining : unit -> unit
  val new_leader : Game.Leader.t list -> unit
  val ranger : unit -> unit
  val res : unit -> unit
  val templar : unit -> unit
  val units : unit -> unit
end

module With : Game.State.S -> S
