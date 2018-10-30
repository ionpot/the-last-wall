let cost_from scouting mp =
  let cost = if scouting then 10 else 0 in
  mp + cost

let check_starvation sp =
  if sp < 0 then Some ~-sp else None
