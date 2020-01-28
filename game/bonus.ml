module Attr = Units.Attr

module Make (S : State.S) = struct
  let barracks kind =
    S.Nation.check (Nation.has_barracks kind)

  let barraged () =
    S.Barrage.check Barrage.can_barrage

  let bld_ready kind =
    S.Build.check (Build.is_ready kind)

  let deity = S.Deity.is

  let hit_run () =
    S.Barrage.check Barrage.can_hit_run

  let ldr_alive () =
    S.Leader.check (Leader.is_alive)

  let ldr_defense () =
    S.Leader.return (Leader.defense_of)

  let ldr_is kind =
    S.Leader.check (Leader.is_living kind)

  let ldr_cha kind =
    Number.if_ok (S.Leader.return Leader.cha_mod_of) (ldr_is kind)

  let mishap kind =
    S.Mishap.check (Mishap.has kind)

  let researched kind =
    S.Research.check (Research.is_complete kind)

  let trade kind =
    S.Nation.check (Nation.has_trade kind)

  let traded kind =
    S.Nation.check (Nation.has_traded kind)

  let weather = S.Weather.is

  let artillery p =
    if ldr_is Leader.Engineer
    then Power.attr Attr.siege 1. p
    else p

  let brg_coef c = c
    |> Float.add_if (weather Weather.Clear) 0.02
    |> Float.add_if (traded Nation.Numendor) 0.02

  let brg_penalty p =
    let count_ok =
      S.Units.return Units.(filter_count Attr.hit_run)
      >= S.Enemy.return Units.(filter_count Attr.flying)
    in
    if barraged () || (hit_run () && count_ok)
    then Power.attr Attr.flying ~-.1. p
    else p

  let brg_power p = p
    |> Power.add Units.Ranger 1.
    |> Power.inc_by Units.Xbowman 0.5

  let build_cost kind res =
    let engrs = bld_ready Build.Engrs in
    let elanis = deity Deity.Elanis in
    let stable = kind = Build.Stable in
    res
    |> Resource.bonus_if (elanis && stable)
      Resource.Bonus.(Sub (Both 0.2))
    |> Resource.bonus_if engrs
      Resource.Bonus.(Sub (Sup 0.1))

  let cav_allowed ratio = ratio
    |> Float.add_if (deity Deity.Elanis) 0.1
    |> Float.add_if (traded Nation.Tulron) 0.1

  let dr_leader dr = dr
    |> Float.add_if (ldr_alive ()) (ldr_defense ())

  let dr_mausoleums dr =
    let n = S.Build.return Build.mausoleums in
    let bonus = Float.if_ok 0.01 (deity Deity.Lerota) in
    dr +. (float n *. (0.01 +. bonus))

  let dr_penalties dr = dr
    |> Float.ssub_if (barraged ()) 0.05
    |> Float.ssub_if (mishap Mishap.Comet) 0.05
    |> Float.ssub_if (weather Weather.Heat) 0.02

  let dr_snow_penalty p =
    if weather Weather.(Snow Heavy)
    then Power.set_attr Attr.cavalry 0. p
    else p

  let dr_wind_penalty p =
    if weather Weather.Wind
    then Power.set_attr Attr.flying 0. p
    else p

  let market_boost kind =
    let cha = ldr_cha Leader.Merchant in
    let ratio = float cha *. 0.1 in
    Resource.bonus_if (kind = Build.Market)
    Resource.Bonus.(Add (Sup ratio))

  let recruit_fast kind =
    if Attr.(is siege) kind
    then ldr_is Leader.Engineer
    else false

  let recruit_sup kind =
    let ratio = 0.
      |> Float.add_if (ldr_is Leader.Aristocrat && Attr.(is cavalry) kind) 0.2
      |> Float.add_if (ldr_is Leader.Merchant && kind = Units.Merc) 0.1
      |> Float.add_if (traded Nation.Clan && Attr.(is siege) kind) 0.2
    in Number.reduce_by ratio

  let resource_disease res =
    let disease = Float.if_ok 0.2 (mishap Mishap.Disease) in
    Resource.(bonus Bonus.(Sub (Both disease))) res

  let siege_boost p =
    if traded Nation.Clan
    then Power.attr Attr.siege 1. p
    else p

  let smite mnp = mnp
    |> Float.add_if (bld_ready Build.Observatory) 10.

  let support_barracks kind res =
    let mnp = Number.if_ok 10 (barracks kind) in
    Resource.add ~mnp res

  let support_hekatium kind =
    let hekatium = kind = Nation.Hekatium in
    Resource.bonus_if (trade kind && hekatium)
    Resource.Bonus.(Add (Both 0.1))

  let support_trade kind res =
    let sup = Number.if_ok 10 (trade kind) in
    Resource.add ~sup res

  let support_winter chance =
    let winter = S.Month.check Month.is_winter in
    Number.sub_if winter 10 chance

  let temple_men n =
    Number.add_if (bld_ready Build.Guesthouse) 1 n

  let upkeep_engr sup =
    let cha = ldr_cha Leader.Engineer in
    Number.reduce_by (float cha *. 0.03) sup

  let upkeep_scouting sup =
    let cavs = S.Units.return Units.(count Cavalry) in
    Number.reduce_by (float cavs *. 0.02) sup

  let upkeep_units sup =
    let discount ratio fn =
      S.Units.return fn |> Units.upkeep |> Number.portion ratio
    in
    let cavs = Float.if_ok 0.2 (traded Nation.Tulron) in
    let merc = Float.if_ok 0.1 (researched Research.BlackArmy) in
    sup
    - discount cavs Units.(filter Attr.cavalry)
    - discount merc Units.(only Units.Merc)

  let volunteers n =
    n + (ldr_cha Leader.Aristocrat)
end
