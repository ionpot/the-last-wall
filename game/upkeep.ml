module Make (M : State.S) = struct
  let men = M.get_manp ()
  let cav = M.Cavalry.get ()
  let scouts = M.Scout.either 10 0
  let value = men + cav + scouts
end

module Starvation (M : State.S) = struct
  let cost = -M.get_supp ()
  let sup, men = Number.take cost (M.get_manp ())
  let cav = min sup (M.Cavalry.get ())
  let value = men, cav
end
