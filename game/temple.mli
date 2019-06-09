module With : State.S -> sig
  val cap_rem : unit -> Defs.count
  val cap_for : Units.kind -> Defs.count
  val promotable : Units.kind -> Defs.count

  val buy : Units.kind -> Defs.count -> unit
  val promote : Units.kind -> Defs.count -> unit
end
