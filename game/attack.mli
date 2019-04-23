type report =
  | Accurate of Units.report
  | Vague of Units.sum_report

module Roll : Dice.S -> sig
  val attack : Defs.turn -> Units.t
  val report : Defs.scouting -> Units.t -> report
end
