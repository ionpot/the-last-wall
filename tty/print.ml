open Convert
open Game
open Printf

module Build = struct
  let status nat (ready, built, queued) =
    Tty.ifpairln "buildings ready" (bld_ls2str nat ready);
    Tty.ifpairln "build finished" (bld_ls2str nat built);
    Tty.ifpairln "build queue" (bld_q2str nat queued)

  let all nat t =
    bld_map2str nat (Build.ready t)
    |> Tty.ifpairln "buildings ready";
    status nat Build.([], built t, queue t)

  let manp mnp =
    if mnp > 0 then
      Tty.pairln "construction" (work2str mnp)

  let supply sup =
    if sup > 0 then
      sprintf "construction costs %s" (sup2str sup)
      |> Tty.writeln
end

module Leader = struct
  let died t =
    sprintf
      "%s has died, it was a grim sacrifice to protect the wall"
      (ldr2first t)
    |> Tty.writeln

  let check f t =
    if Leader.is_alive t then f t else ""

  let to_first = check ldr2first
  let to_full = check ldr2full
  let to_name = check ldr2name

  let to_fled t units =
    let units = units2str units in
    match to_first t, units with
    | "", "" -> ""
    | "", str
    | str, "" -> sprintf "%s has fled" str
    | ldr, units -> sprintf "%s has fled with %s" ldr units

  let lvup t =
    sprintf "%s is now %s" (ldr2first t) (ldr2status t)
    |> Tty.writeln
end

module Combat = struct
  let begins bonus units enemies ldr =
    let base = Power.base bonus in
    Tty.lnwriteln "combat phase";
    Tty.pairln "attacking" (units2str enemies |> str2none);
    Tty.pairln "defending" (units2mnpstr base units);
    Tty.ifpairln "leader" (Leader.to_full ldr)

  let cav_ratio ratio allowed too_many =
    if ratio > 0. then
      sprintf "%s / %s"
        (percent2str ratio) (percent2str allowed)
      |> Tty.pairln "cav ratio";
    if too_many
    then Tty.writeln "too many cavalry, defense reduced"

  let stats atk def dmg =
    let attack = sprintf "%s attack" (power2str atk) in
    let defense = sprintf "%s dr" (percent2str def) in
    let damage = sprintf "%s damage" (power2str dmg) in
    sprintf "%s -> %s -> %s" attack defense damage

  let retreat ldr fled casualty =
    Tty.writeln (Leader.to_fled ldr fled);
    let str = result2remaining casualty in
    if str <> "" then sprintf "%s went rogue" str |> Tty.writeln

  let outcome (module O : Combat.Outcome) ldr =
    Tty.writeln "enemies attack";
    cav_ratio O.cav_ratio O.cav_allowed O.cav_too_many;
    Tty.writeln (stats O.attack O.defense O.damage);
    Tty.ifwriteln (result2stats O.casualty);
    Tty.pairln "casualty" (result2outcome O.casualty |> str2none);
    if O.retreat then retreat ldr O.fled O.casualty;
    if O.ldr_died then Leader.died ldr;
    Tty.ifpairln "enemies remaining" (result2remaining O.enemies)
end

let barrage_status w =
  let open Barrage in
  function
    | Available -> ()
    | Disabled Archers -> Tty.writeln "no archers for arrow barrage"
    | Disabled Leader -> Tty.writeln "no leader to lead arrow barrage"
    | Disabled Weather -> Tty.spln (weather2str w) "prevents arrow barrage"

let disease (died, ldr_died) ldr =
  Tty.pairln "died" (units2str died |> str2none);
  if ldr_died then Leader.died ldr

let facilities nat map =
  facs2clean map
  |> facs2str nat
  |> Tty.ifpairln "facilities"

let fear fled =
  if units2bool fled
  then sprintf "%s fled in fear" (units2str fled) |> Tty.writeln

let fear_end fled =
  if units2bool fled
  then sprintf "%s returns" (units2str fled) |> Tty.writeln

let hit_run (killed, died) =
  if units2bool killed
  then sprintf "horse archers kill %s" (units2str killed) |> Tty.writeln;
  if units2bool died
  then sprintf "lost %s" (units2str died) |> Tty.writeln

let mishap t =
  let print kind =
    mishap2str kind
    |> sprintf "!!! %s !!!"
    |> Tty.writeln
  in
  let f kind = if Mishap.has kind t then print kind in
  List.iter f Mishap.kinds

let research_progress (module S : Research.Progress) =
  Tty.ifpairln "researching" (researchset2str S.started)

let research_status s =
  Tty.ifpairln "research complete" (researchset2str s)

let siege kind (units, _) =
  if units2bool units then
    sprintf "%s kills %s"
    (unit2str kind) (units2str units |> if_empty "nothing")
    |> Tty.writeln

let starting nat (module S : Starting.S) =
  Tty.ifpairln "buildings" (bld_ls2str nat S.buildings);
  Tty.pairln "month" (month2str S.month);
  Tty.pairln "supply" (sup2str S.supply);
  Tty.ifpairln "units" (units2str S.units)

let starvation (fled, starved) =
  Tty.ifpairln "starved" (units2str starved);
  Tty.ifpairln "fled" (units2str fled)

let support s =
  let to_str nat res =
    sprintf "%s sent %s" (nation2str nat) (res2nothing res)
  in
  let module Map = Nation.Map in
  Nation.kinds
  |> List.filter (fun k -> Map.mem k s)
  |> List.map (fun k -> to_str k (Map.find k s))
  |> Tty.writelns
