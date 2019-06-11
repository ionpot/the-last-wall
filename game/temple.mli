module With : State.S -> sig
  val cap_rem : unit -> Defs.count
  val cap_for : Units.kind -> Defs.count
  val dervish_range : unit -> Defs.count * Defs.count
  val promotable : unit -> Defs.count

  val buy : Units.kind -> Defs.count -> unit
  val promote : Units.kind -> Defs.count -> unit
end
