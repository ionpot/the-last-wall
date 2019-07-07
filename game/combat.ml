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
      S.Casualty.map (Units.combine remaining);
      S.Units.set O.units;
      S.Build.map Build.(raze Fort)
    end else begin
      S.Casualty.map (Units.combine O.units);
      S.Units.set remaining
    end;
    S.Enemy.map (Units.reduce O.enemies);
    if O.ldr_died then begin
      S.Leader.map (S.Turn.return Leader.died);
      S.Build.map (S.Leader.return Build.died)
    end
end

let fort_cap = 20.

let to_power = Defs.to_power

let heal kind ls =
  let f = Float.floor_by (Units.base_power kind) in
  List.map (fun (k, n) -> k, if k = kind then f n else n) ls

let after ls =
  heal Units.Templar ls

let countered units enemies =
  if Units.has_base_power 2. units
  then enemies
  else Units.(rm Cyclops) enemies

let to_units t ls =
  Units.Picked.(groupf_by t ls |> to_units)
  |> Units.clean

module Units (S : State.S) = struct
  module Dist = Units.Dist(S.Dice)
  module Fill = Units.Fill(S.Dice)
  let attack = S.Enemy.return Units.power
  let harpies = S.Enemy.return Units.(count Harpy)
  let defending = S.Units.get ()
  let units = Units.(rm Ballista) defending
  let power = Units.power units
  let fled () = Fill.from fort_cap units
  let fought () = Float.sub power fort_cap
  let lost dmg =
    Dist.from dmg defending |> after |> to_units defending
  let enemy_loss dmg =
    let e = S.Enemy.return (countered units) in
    Dist.from dmg e |> to_units e
end

module Make (S : State.S) = struct
  module Dr = Dr.From(S)
  module LdrRoll = Leader.Roll(S.Dice)
  module Units = Units(S)

  let harpy_weaken = to_power Units.harpies (S.Barraging.either 1. 0.)

  let value = (module struct
    let cav_too_many = Dr.cav_too_many
    let attack = Units.attack -. harpy_weaken
    let defense = Dr.value
    let damage = Float.reduce attack defense
    let defeat = damage > Units.power
    let retreat = defeat && S.Build.check Build.(ready Fort)
    let power = if retreat then Units.fought () else Units.power
    let units = if retreat then Units.fled () else Units.lost damage
    let enemies = Units.enemy_loss power
    let ldr_died =
      if retreat then false
      else S.Leader.check LdrRoll.death
  end : Outcome)
end
