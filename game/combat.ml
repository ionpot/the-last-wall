module type Outcome = sig
  val attack : Defs.power
  val cav_too_many : bool
  val damage : Defs.power
  val defense : Defs.power
  val enemies : Units.t
  val ldr_died : bool
  val retreat : bool
  val units : Units.t
end

module Apply (S : State.S) = struct
  let value (module O : Outcome) =
    let remaining = S.Units.return (Units.reduce O.units) in
    if O.retreat then begin
      S.Casualty.set remaining;
      S.Units.set O.units;
      S.Build.map Build.(raze Fort)
    end else begin
      S.Casualty.set O.units;
      S.Units.set remaining
    end;
    S.Enemy.map (Units.reduce O.enemies);
    if O.ldr_died then begin
      S.Leader.map (S.Turn.return Leader.died);
      S.Build.map (S.Leader.return Build.died)
    end
end

let barrage_dr = 0.05
let cav_unit_dr = 0.002
let cav_dr_penalty = 0.05
let cav_men_ratio = 0.4
let fort_cap = 20.
let harpy_dr = 0.002
let mausoleum_dr = 0.01

let floor_harpy_dr = Float.floor_by 0.01
let to_power = Defs.to_power

let cav_dr cav too_many snow =
  if too_many then -.cav_dr_penalty
  else if snow then 0.
  else to_power cav cav_unit_dr

module Units (S : State.S) = struct
  module Roll = Units.Roll(S.Dice)
  let attack = S.Enemy.return Units.power
  let harpies = S.Enemy.return Units.(count Harpy)
  let cavs = S.Units.return Units.(count Cavalry)
  let snow = S.Weather.is Weather.(Snow Heavy)
  let ratio = S.Units.return Units.(ratio Cavalry Men)
  let ratio_bonus = if S.Deity.is Deity.Elanis then 0.1 else 0.
  let cav_too_many = ratio > cav_men_ratio +. ratio_bonus
  let cav_dr = cav_dr cavs cav_too_many snow
  let power = S.Units.return Units.power
  let fled () = S.Units.return (Roll.pick fort_cap)
  let fought () = Float.sub power fort_cap
  let lost dmg = S.Units.return (Roll.pick dmg)
  let enemy_loss dmg = S.Enemy.return (Roll.pick dmg)
end

module Make (S : State.S) = struct
  module Roll = struct
    module Leader = Leader.Roll(S.Dice)
  end
  module Units = Units(S)
  let ldr_alive = S.Leader.check Leader.is_alive
  let ldr_dr = S.Leader.return Leader.defense_of
  let barrage_dr =
    if ldr_alive then S.Barraging.either barrage_dr 0. else 0.
  let harpy_dr = floor_harpy_dr (to_power Units.harpies harpy_dr)
  let harpy_weaken = to_power Units.harpies (S.Barraging.either 1. 0.)
  let mausoleums = S.Build.return Build.mausoleums
  let mausoleum_dr =
    let bonus = if S.Deity.is Deity.Lerota then 2 else 1 in
    to_power (mausoleums * bonus) mausoleum_dr

  let value = (module struct
    let cav_too_many = Units.cav_too_many
    let attack = Units.attack -. harpy_weaken
    let defense =
      Units.cav_dr +. ldr_dr +. mausoleum_dr -. barrage_dr -. harpy_dr
    let damage = Float.reduce attack defense
    let defeat = damage > Units.power
    let retreat = defeat && S.Build.check Build.(ready Fort)
    let power = if retreat then Units.fought () else Units.power
    let units = if retreat then Units.fled () else Units.lost damage
    let enemies = Units.enemy_loss power
    let ldr_died =
      if retreat then false
      else S.Leader.check Roll.Leader.death
  end : Outcome)
end
