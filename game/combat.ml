module Dist = Units.Dist

module type Outcome = sig
  val attack : Defs.power
  val cav_too_many : bool
  val damage : Defs.power
  val defense : Defs.power
  val enemies : Dist.result
  val fled : (Units.t * Units.t) option
  val ldr_died : bool
  val units : Dist.result
end

module Apply (S : State.S) = struct
  let value (module O : Outcome) =
    S.Enemy.map (O.enemies |> Dist.outcome |> Units.reduce);
    if O.ldr_died then begin
      S.Leader.map (S.Turn.return Leader.died);
      S.Build.map (S.Leader.return Build.died)
    end;
    match O.fled with
    | Some (fled, remaining) ->
        S.Casualty.map (Units.combine remaining);
        S.Units.set fled;
        S.Build.map Build.(raze Fort)
    | None ->
        S.Casualty.map (O.units |> Dist.outcome |> Units.combine);
        S.Units.set (Dist.remaining O.units)
end

let fort_cap = 20.

module Units (S : State.S) = struct
  module DistRoll = Dist.Roll(S.Dice)
  module Fill = Units.Fill(S.Dice)
  let enemies = S.Enemy.get ()
  let attack = Units.power enemies
  let harpies = Units.(count Harpy) enemies
  let units = S.Units.get ()
  let defending = Units.(discard Attr.is_siege) units
  let power = Units.power units
  let fled () =
    let fled = Fill.from fort_cap defending in
    fled, Units.reduce fled units
  let fought () = Float.sub power fort_cap
  let lost dmg = DistRoll.from dmg units
  let enemy_loss dmg =
    Units.countered units enemies
    |> DistRoll.from dmg
end

module Make (S : State.S) = struct
  module Dr = Dr.From(S)
  module LdrRoll = Leader.Roll(S.Dice)
  module Units = Units(S)

  let harpy_weaken =
    S.Barraging.either 1. 0.
    |> Defs.to_power Units.harpies

  let have_fort =
    S.Build.check Build.(ready Fort)

  let value = (module struct
    let cav_too_many = Dr.cav_too_many
    let attack = Units.attack -. harpy_weaken
    let defense = Dr.value
    let damage = Float.reduce attack defense
    let retreat = damage > Units.power && have_fort
    let fled = if retreat then Some (Units.fled ()) else None
    let power = if retreat then Units.fought () else Units.power
    let units = if retreat then Dist.empty else Units.lost damage
    let enemies = Units.enemy_loss (Float.increase power defense)
    let ldr_died =
      if retreat then false
      else S.Leader.check LdrRoll.death
  end : Outcome)
end
