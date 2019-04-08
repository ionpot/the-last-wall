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
      S.bld_raze Building.Fort
    end else begin
      S.Men.sub men;
      S.Cavalry.sub cav
    end;
    S.Enemy.set O.remaining;
    if O.ldr_died
    then S.Leader.map (S.Turn.return Leader.died)
end

let barrage_dr = 0.05
let cav_str = Cavalry.strength
let fort_cap = 20.
let men_str = 1.

let to_ls men cav =
  [men, men_str; cav, cav_str]

let picked = function
  | [men; cav] -> men, cav
  | ls -> Pick.invalid ls

let (--) a b =
  if a < b then 0. else a -. b

module Units (S : State.S) = struct
  let cav = S.Cavalry.get ()
  let men = S.Men.get ()
  let cav_dr = S.Cavalry.return (Cavalry.dr men)
  let cav_too_many = S.Cavalry.return (Cavalry.too_many men)
  let power = Defs.to_power men men_str +. Defs.to_power cav cav_str
  let fled () =
    to_ls men cav |> Pick.units fort_cap |> picked
  let fought () = power -- fort_cap
  let lost dmg =
    to_ls men cav |> Pick.units dmg |> picked
end

module Make (S : State.S) = struct
  module Units = Units(S)
  let ldr_alive = S.Leader.check Leader.is_alive
  let ldr_dr = S.Leader.return Leader.defense_of
  let barrage_dr =
    if ldr_alive then S.Barraging.either barrage_dr 0. else 0.

  let value = (module struct
    let cav_too_many = Units.cav_too_many
    let attack = S.Enemy.return Enemy.damage
    let defense = Units.cav_dr +. ldr_dr -. barrage_dr
    let damage = attack -. attack *. defense
    let defeat = damage > Units.power
    let retreat = defeat && S.bld_ready Building.Fort
    let power = if retreat then Units.fought () else Units.power
    let units = if retreat then Units.fled () else Units.lost damage
    let remaining = S.Enemy.return (Enemy.discard power)
    let ldr_died =
      if retreat then false
      else S.Leader.return Leader.has_died
  end : Outcome)
end
