module With : State.S -> sig
  open Defs

  module Missing : sig
    val arena : unit -> count
    val stable : unit -> count
    val temple : unit -> count
  end

  val affordable : Units.kind -> count -> count
  val dervish_range : unit -> count range
  val promotable : Units.kind -> count
  val supply_limit : Units.kind -> count -> count

  val promote : Units.kind -> count -> unit
  val sub_cost : Units.kind -> count -> unit
end
