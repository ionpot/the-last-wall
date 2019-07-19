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

module Units (S : State.S) = struct
  module Dist = Units.Dist(S.Dice)
  module Fill = Units.Fill(S.Dice)
  let enemies = S.Enemy.get ()
  let attack = Units.power enemies
  let harpies = Units.(count Harpy) enemies
  let units = S.Units.get ()
  let defending = Units.defending units
  let power = Units.power defending
  let fled () = Fill.from fort_cap defending
  let fought () = Float.sub power fort_cap
  let lost dmg = Dist.from dmg units
  let enemy_loss dmg =
    Units.countered units enemies
    |> Dist.from dmg
end

module Make (S : State.S) = struct
  module Dr = Dr.From(S)
  module LdrRoll = Leader.Roll(S.Dice)
  module Units = Units(S)

  let harpy_weaken =
    S.Barraging.either 1. 0.
    |> Defs.to_power Units.harpies

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
