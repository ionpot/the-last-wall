let cav_dr_penalty = 0.05
let cav_men_ratio = 0.4

module From (S : State.S) = struct
  module Bonus = Bonus.Make(S)

  let units = S.Units.get ()
  let cavs = Units.(filter Attr.cavalry) units
  let infantry = Units.(filter_count Attr.infantry) units
  let cav_ratio = Number.ratio (Units.count_all cavs) infantry
  let cav_allowed = Bonus.cav_allowed cav_men_ratio
  let cav_too_many = cav_ratio > cav_allowed
  let cav_dr =
    if cav_too_many then -.cav_dr_penalty
    else Power.dr cavs |> Bonus.dr_snow_penalty |> Power.of_units cavs

  let enemy_dr =
    let enemy = S.Enemy.get () in
    Power.dr enemy
    |> Bonus.dr_wind_penalty
    |> Power.of_units enemy

  let value =
      cav_dr
      |> Bonus.dr_leader
      |> Bonus.dr_mausoleums
      |> Float.ssub_by enemy_dr
      |> Bonus.dr_penalties
end
