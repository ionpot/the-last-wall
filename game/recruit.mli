module With : State.S -> sig
  module Missing : sig
    val arena : unit -> Defs.count
    val stable : unit -> Defs.count
    val temple : unit -> Defs.count
  end

  val affordable : Units.kind -> Defs.count -> Defs.count
  val dervish_range : unit -> Defs.count * Defs.count
  val promotable : Units.kind -> Defs.count
  val supply_limit : Units.kind -> Defs.count -> Defs.count

  val promote : Units.kind -> Defs.count -> unit
  val sub_cost : Units.kind -> Defs.count -> unit
end
