type report =
  | Accurate of Units.report
  | Vague of Units.sum_report

module Make : State.S -> sig
  val roll : Defs.turn -> Units.t
  val report : Defs.scouting -> Units.t -> report
end
