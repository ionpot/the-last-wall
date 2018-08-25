type report =
  | Report of Enemy.report
  | SumReport of Enemy.sum_report

module Make (M : State.S) = struct
  let get_cost () =
    if M.is_scouting ()
    then Resource.of_supp 10
    else Resource.empty

  let get_report () =
    let e = M.get_enemies () in
    if M.is_scouting ()
    then Report (Enemy.report_of e)
    else SumReport (Enemy.sum_report_of e)
end
