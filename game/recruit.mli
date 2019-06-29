module With : State.S -> sig
  val affordable : Units.kind -> Defs.count -> Defs.count
  val dervish_range : unit -> Defs.count * Defs.count
  val stable_cap : unit -> Defs.count
  val temple_cap : unit -> Defs.count

  val buy : Units.kind -> Defs.count -> unit
  val promote : Units.kind -> Defs.count -> unit
end
