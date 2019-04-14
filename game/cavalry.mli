module Check : State.S -> sig
  val value : bool
end

module Make : State.S -> sig
  val value : Defs.count
end
