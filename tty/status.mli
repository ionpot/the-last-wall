open Game

module type S = sig
  val cavalry : Defs.count -> unit
  val dervish : unit -> unit
  val enemies : unit -> unit
  val facilities : Direct.Facilities.t -> unit
  val leader : unit -> unit
  val new_leader : Input.LeaderNew.t -> unit
  val promote : Units.kind -> unit
  val res : unit -> unit
  val units : unit -> unit
end

module With : State.S -> S
