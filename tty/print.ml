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

  let manp need bonus units =
    let base = Power.base bonus in
    if need > 0 then
    units2work base units
    |> min need
    |> manp2str
    |> sprintf "%s for construction"
    |> Tty.writeln

  let supply need avlb =
    if need > 0 then
    sprintf "construction costs %s, have %s" (sup2str need) (sup2str avlb)
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
    if O.cav_too_many
    then Tty.writeln "too many cavalry, defense reduced";
    Tty.writeln (stats O.attack O.defense O.damage);
    Tty.ifwriteln (result2stats O.casualty);
    Tty.pairln "casualty" (result2outcome O.casualty |> str2none);
    if O.retreat then retreat ldr O.fled O.casualty;
    if O.ldr_died then Leader.died ldr;
    Tty.ifpairln "enemies remaining" (result2remaining O.enemies)
end

let ballista (n, enemies, _) =
  if n > 0 then
  sprintf "%d ballista kills %s" n (units2str enemies |> if_empty "nothing")
  |> Tty.writeln

let barrage_status w =
  let open Barrage in
  function
    | Available -> ()
    | Disabled Leader -> Tty.writeln "no leader to lead arrow barrage"
    | Disabled Weather -> Tty.spln (weather2str w) "prevents arrow barrage"

let cyclops (n, units, _) =
  if n > 0 then
  sprintf "%d cyclops kills %s" n (units2str units |> if_empty "nothing")
  |> Tty.writeln

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

let mishap t =
  let print kind =
    mishap2str kind
    |> sprintf "!!! %s !!!"
    |> Tty.writeln
  in
  let f kind = if Mishap.has kind t then print kind in
  List.iter f Mishap.kinds

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
