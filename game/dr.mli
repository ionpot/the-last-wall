module From : State.S -> sig
  val cav_allowed : float
  val cav_ratio : float
  val cav_too_many : bool
  val value : Defs.power
end
