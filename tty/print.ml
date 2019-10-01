open Convert
open Game
open Printf

module Build = struct
  let status (ready, built, queued) =
    Tty.ifpairln "buildings ready" (bld_ls2str ready);
    Tty.ifpairln "build finished" (bld_ls2str built);
    Tty.ifpairln "build queue" (bld_q2str queued)

  let all t =
    bld_map2str (Build.ready t)
    |> Tty.ifpairln "buildings ready";
    status Build.([], built t, queue t)

  let manp need units =
    if need > 0 then
    units2work units
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
  let begins units enemies ldr =
    Tty.lnwriteln "combat phase";
    Tty.pairln "attacking" (units2str enemies |> str2none);
    Tty.pairln "defending" (units2mnpstr units);
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

let ballista (n, enemies) =
  if n > 0 then
  sprintf "%d ballista kills %s" n (units2str enemies |> if_empty "nothing")
  |> Tty.writeln

let can_barrage w =
  let open Direct.CanBarrage in
  function
    | No Leader -> Tty.writeln "no leader to lead arrow barrage"
    | No Weather -> Tty.spln (weather2str w) "prevents arrow barrage"
    | Yes -> ()

let cyclops (n, units) =
  if n > 0 then
  sprintf "%d cyclops kills %s" n (units2str units |> if_empty "nothing")
  |> Tty.writeln

let disease (units, died) ldr =
  Tty.writeln "!!! disease outbreak !!!";
  Tty.pairln "died" (units2str units |> str2none);
  if died then Leader.died ldr

let starting (module S : Starting.S) =
  Tty.ifpairln "buildings" (bld_ls2str S.buildings);
  Tty.pairln "month" (month2str S.month);
  Tty.pairln "supply" (sup2str S.supply);
  Tty.ifpairln "units" (units2str S.units)

let support s =
  let f res =
    if res = Resource.empty then "nothing"
    else res2str res
  in
  Support.ls s
  |> List.map (fun (nat, res) -> (nation2str nat, f res))
  |> List.map (fun (nat, res) -> sprintf "%s sent %s" nat res)
  |> Tty.writelns
