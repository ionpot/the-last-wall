module Make (S : State.S) = struct
  let men = S.Men.get ()
  let cav = S.Cavalry.get ()
  let scouts = S.Scout.either 10 0
  let value = men + cav + scouts
end

module Starvation (S : State.S) = struct
  let cost = -S.Supply.get ()
  let sup, men = S.Men.return (Number.take cost)
  let cav = min sup (S.Cavalry.get ())
  let value = men, cav
end
