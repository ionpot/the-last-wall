let barrage_dr = 0.05
let cav_unit_dr = 0.002
let cav_dr_penalty = 0.05
let cav_men_ratio = 0.4
let harpy_dr = 0.002
let mausoleum_dr = 0.01

let floor_harpy_dr = Float.floor_by 0.01
let to_power = Defs.to_power

let cav_dr cav too_many snow =
  if too_many then -.cav_dr_penalty
  else if snow then 0.
  else to_power cav cav_unit_dr

module From (S : State.S) = struct
  let cavs = S.Units.return Units.(count Cavalry)
  let snow = S.Weather.is Weather.(Snow Heavy)
  let infantry = S.Units.return Units.count_infantry
  let ratio = Number.ratio cavs infantry
  let ratio_bonus = if S.Deity.is Deity.Elanis then 0.1 else 0.
  let cav_too_many = ratio > cav_men_ratio +. ratio_bonus
  let cav_dr = cav_dr cavs cav_too_many snow

  let ldr_alive = S.Leader.check Leader.is_alive
  let ldr_dr =
    if ldr_alive then S.Leader.return Leader.defense_of else 0.

  let mausoleums = S.Build.return Build.mausoleums
  let mausoleum_dr =
    let bonus = if S.Deity.is Deity.Lerota then 2 else 1 in
    to_power (mausoleums * bonus) mausoleum_dr

  let barrage_dr =
    if ldr_alive then S.Barraging.either barrage_dr 0. else 0.

  let harpies = S.Enemy.return Units.(count Harpy)
  let harpy_dr = floor_harpy_dr (to_power harpies harpy_dr)

  let value =
      cav_dr +. ldr_dr +. mausoleum_dr -. barrage_dr -. harpy_dr
end
