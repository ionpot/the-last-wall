module type Outcome = sig
  val attack : Defs.power
  val casualty : Dist.t
  val cav_allowed : float
  val cav_ratio : float
  val cav_too_many : bool
  val damage : Defs.power
  val defense : Defs.power
  val enemies : Dist.t
  val fled : Units.t
  val ldr_died : bool
  val retreat : bool
end

type t = (module Outcome)

module Apply (S : State.S) = struct
  module LdrDied = Event.LdrDied(S)
  let value (module O : Outcome) =
    S.Casualty.map (O.casualty |> Dist.outcome |> Units.combine);
    S.Enemy.set (Dist.remaining O.enemies);
    S.Units.set O.fled;
    if O.retreat then S.Build.map Build.(raze Fort);
    if O.ldr_died then LdrDied.value 2
end

let fort_cap = 20.

module Make (S : State.S) = struct
  module Bonus = Bonus.Make(S)
  module Damage = Dist.Damage(S.Dice)
  module Dr = Dr.From(S)
  module Fill = Dist.Fill(S.Dice)
  module LdrRoll = Leader.Roll(S.Dice)

  let a_power = Power.base |> Bonus.brg_penalty
  let d_power = Power.base |> Bonus.siege_boost

  let enemies = S.Enemy.get ()
  let have_fort = S.Build.check Build.(is_ready Fort)
  let units = S.Units.get ()
  let mobile = Units.(discard Attr.siege) units

  let dist_atk dmg a b =
    let module D = Damage(struct
      let full_absorb = true
      let use_ratio = false
    end) in
    D.from dmg d_power a b

  let dist_dfn dmg a b =
    let module D = Damage(struct
      let full_absorb = false
      let use_ratio = true
    end) in
    D.from dmg a_power a b

  let value = (module struct
    let cav_allowed = Dr.cav_allowed
    let cav_ratio = Dr.cav_ratio
    let cav_too_many = Dr.cav_too_many
    let attack = Power.of_units enemies a_power
    let defense = Dr.value
    let damage = Float.reduce attack defense
    let result = dist_dfn damage enemies units
    let retreat = Dist.no_remaining result && have_fort
    let fled, fought =
      if retreat
      then Fill.from fort_cap d_power mobile
      else Dist.remaining result, units
    let power = Power.of_units fought d_power
    let casualty =
      if retreat
      then dist_dfn power enemies fought
      else result
    let enemies =
      let refl = Dist.reflected casualty in
      let dmg = Float.increase power defense in
      dist_atk (dmg +. refl) fought enemies
    let ldr_died =
      if retreat then false
      else S.Leader.check LdrRoll.death
  end : Outcome)
end
