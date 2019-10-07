let barrage_dr = 0.05
let cav_dr_penalty = 0.05
let cav_men_ratio = 0.4
let heat_penalty = 0.02
let mausoleum_dr = 0.01

let cav_dr too_many snow units =
  if too_many then -.cav_dr_penalty
  else snow |> Float.if_not Units.(filter Attr.is_cavalry units |> dr)

module From (S : State.S) = struct
  let barraging = S.Barraging.get ()
  let heat = S.Weather.is Weather.Heat
  let snow = S.Weather.is Weather.(Snow Heavy)
  let wind = S.Weather.is Weather.Wind

  let cavs = S.Units.return Units.(filter_count Attr.is_cavalry)
  let infantry = S.Units.return Units.(filter_count Attr.is_infantry)
  let ratio = Number.ratio cavs infantry
  let ratio_bonus = Float.if_ok 0.1 (S.Deity.is Deity.Elanis)
  let cav_too_many = ratio > cav_men_ratio +. ratio_bonus
  let cav_dr = S.Units.return (cav_dr cav_too_many snow)

  let ldr_alive = S.Leader.check Leader.is_alive
  let ldr_dr = S.Leader.return Leader.defense_of

  let mausoleums = S.Build.return Build.mausoleums
  let mausoleum_dr =
    let bonus = if S.Deity.is Deity.Lerota then 2 else 1 in
    Float.times (mausoleums * bonus) mausoleum_dr

  let enemy_dr =
    let harpy, rest = S.Enemy.return Units.(split Harpy) in
    Units.dr rest |> Float.add_if (not wind) (Units.dr harpy)

  let value =
      cav_dr
      |> Float.add_if ldr_alive ldr_dr
      |> (+.) mausoleum_dr
      |> Float.sub_if barraging barrage_dr
      |> Float.sub_by enemy_dr
      |> Float.sub_if heat heat_penalty
end
