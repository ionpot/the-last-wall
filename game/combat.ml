module type Outcome = sig
  val attack : Defs.power
  val cav_too_many : bool
  val damage : Defs.power
  val defense : Defs.power
  val ldr_died : bool
  val remaining : Enemy.t
  val retreat : bool
  val units : Defs.count * Defs.count
end

module Apply (S : State.S) = struct
  let value (module O : Outcome) =
    let men, cav = O.units in
    if O.retreat then begin
      S.Men.set men;
      S.Cavalry.set cav;
      S.Build.map Build.(raze Fort)
    end else begin
      S.Men.sub men;
      S.Cavalry.sub cav
    end;
    S.Enemy.set O.remaining;
    if O.ldr_died then begin
      S.Leader.map (S.Turn.return Leader.died);
      S.Build.map (S.Leader.return Build.died)
    end
end

let barrage_dr = 0.05
let cav_unit_dr = 0.002
let cav_dr_penalty = 0.05
let cav_men_ratio = 0.4
let cav_str = 2.
let fort_cap = 20.
let mausoleum_dr = 0.01
let men_str = 1.

let to_power = Defs.to_power

let cav_dr cav too_many snow =
  if too_many then -.cav_dr_penalty
  else if snow then 0.
  else to_power cav cav_unit_dr

let picked = function
  | [men; cav] -> men, cav
  | ls -> Pick.invalid ls

let to_ls men cav =
  [men, men_str; cav, cav_str]

let too_many cav men =
  float cav > to_power men cav_men_ratio

let (--) a b =
  if a < b then 0. else a -. b

module Units (S : State.S) = struct
  let cav = S.Cavalry.get ()
  let men = S.Men.get ()
  let snow = S.Weather.is Weather.(Snow Heavy)
  let cav_too_many = too_many cav men
  let cav_dr = cav_dr cav cav_too_many snow
  let power = to_power men men_str +. to_power cav cav_str
  let fled () =
    to_ls men cav |> Pick.random fort_cap |> picked
  let fought () = power -- fort_cap
  let lost dmg =
    to_ls men cav |> Pick.random dmg |> picked
end

module Make (S : State.S) = struct
  module Units = Units(S)
  let ldr_alive = S.Leader.check Leader.is_alive
  let ldr_dr = S.Leader.return Leader.defense_of
  let barrage_dr =
    if ldr_alive then S.Barraging.either barrage_dr 0. else 0.
  let mausoleums = S.Build.return Build.mausoleums
  let mausoleum_dr =
    let bonus = if S.Deity.is Deity.Lerota then 2 else 1 in
    to_power (mausoleums * bonus) mausoleum_dr

  let value = (module struct
    let cav_too_many = Units.cav_too_many
    let attack = S.Enemy.return Enemy.damage
    let defense = Units.cav_dr +. ldr_dr -. barrage_dr +. mausoleum_dr
    let damage = attack -. attack *. defense
    let defeat = damage > Units.power
    let retreat = defeat && S.Build.check Build.(ready Fort)
    let power = if retreat then Units.fought () else Units.power
    let units = if retreat then Units.fled () else Units.lost damage
    let remaining = S.Enemy.return (Enemy.discard power)
    let ldr_died =
      if retreat then false
      else S.Leader.check Leader.roll_death
  end : Outcome)
end
