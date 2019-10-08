module Dist = Units.Dist

module type Outcome = sig
  val attack : Defs.power
  val casualty : Dist.result
  val cav_too_many : bool
  val damage : Defs.power
  val defense : Defs.power
  val enemies : Dist.result
  val fled : Units.t
  val ldr_died : bool
  val retreat : bool
end

type t = (module Outcome)

module Apply (S : State.S) = struct
  let value (module O : Outcome) =
    S.Casualty.map (O.casualty |> Dist.outcome |> Units.combine);
    S.Enemy.set (Dist.remaining O.enemies);
    S.Units.set O.fled;
    if O.retreat then S.Build.map Build.(raze Fort);
    if O.ldr_died then begin
      S.Leader.map (S.Turn.return Leader.died);
      S.Build.map (S.Leader.return Build.died)
    end
end

let fort_cap = 20.

module Units (S : State.S) = struct
  module DistRoll = Dist.Roll(S.Dice)
  module Fill = Units.Fill(S.Dice)
  let dist dmg a b = DistRoll.from dmg (Units.untouchable b a) a
  let enemies = S.Enemy.get ()
  let power = Units.power
  let attack = power enemies
  let harpies = Units.(count Harpy) enemies
  let units = S.Units.get ()
  let defending = Units.(discard Attr.is_siege) units
  let fled () =
    let fled = Fill.from fort_cap defending in
    fled, Units.reduce fled units
end

module Make (S : State.S) = struct
  module Dr = Dr.From(S)
  module LdrRoll = Leader.Roll(S.Dice)
  module Units = Units(S)

  let harpy_weaken =
    Float.if_ok 1.
    |> S.Barraging.return
    |> Float.times Units.harpies

  let have_fort =
    S.Build.check Build.(is_ready Fort)

  let value = (module struct
    let cav_too_many = Dr.cav_too_many
    let attack = Units.attack -. harpy_weaken
    let defense = Dr.value
    let damage = Float.reduce attack defense
    let units = Units.(dist damage units enemies)
    let retreat = Dist.no_remaining units && have_fort
    let fled, fought =
      if retreat then Units.fled ()
      else Dist.remaining units, Units.units
    let power = Units.power fought
    let casualty =
      if retreat
      then Units.dist power fought Units.enemies
      else units
    let enemies =
      let ref = Dist.reflected casualty in
      let dmg = Float.increase power defense in
      Units.(dist (dmg +. ref) enemies) fought
    let ldr_died =
      if retreat then false
      else S.Leader.check LdrRoll.death
  end : Outcome)
end
