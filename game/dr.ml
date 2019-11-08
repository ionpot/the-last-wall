let barrage_dr = 0.05
let cav_dr_penalty = 0.05
let cav_men_ratio = 0.4
let comet_penalty = 0.05
let heat_penalty = 0.02
let mausoleum_dr = 0.01

let cavs_of units =
  Units.(filter Attr.is_cavalry units)

let dr units =
  Power.(dr units |> sum)

let cav_dr too_many snow units =
  if too_many then -.cav_dr_penalty
  else Float.if_not (cavs_of units |> dr) snow

module From (S : State.S) = struct
  let barraging = S.Barrage.check Barrage.is_chosen
  let comet = S.Mishap.check Mishap.(has Comet)
  let heat = S.Weather.is Weather.Heat
  let snow = S.Weather.is Weather.(Snow Heavy)
  let wind = S.Weather.is Weather.Wind

  let units = S.Units.get ()
  let cavs = Units.(filter_count Attr.is_cavalry) units
  let infantry = Units.(filter_count Attr.is_infantry) units
  let ratio = Number.ratio cavs infantry
  let ratio_bonus = Float.if_ok 0.1 (S.Deity.is Deity.Elanis)
  let cav_too_many = ratio > cav_men_ratio +. ratio_bonus
  let cav_dr = cav_dr cav_too_many snow units

  let ldr = S.Leader.get ()
  let ldr_alive = Leader.is_alive ldr
  let ldr_dr = Leader.defense_of ldr

  let mausoleums = S.Build.return Build.mausoleums
  let mausoleum_dr =
    let bonus = if S.Deity.is Deity.Lerota then 2 else 1 in
    Float.times (mausoleums * bonus) mausoleum_dr

  let enemy_dr =
    let harpy, rest = S.Enemy.return Units.(pop Harpy) in
    dr rest |> Float.add_if (not wind) (dr harpy)

  let value =
      cav_dr
      |> Float.add_if ldr_alive ldr_dr
      |> (+.) mausoleum_dr
      |> Float.ssub_if barraging barrage_dr
      |> Float.ssub_by enemy_dr
      |> Float.ssub_if comet comet_penalty
      |> Float.ssub_if heat heat_penalty
end
