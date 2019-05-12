open Convert
open Game
open Printf

module Build = struct
  let status (ready, built, queued) =
    Tty.ifpairln "built" (bld_ls2str ready);
    Tty.ifpairln "finished" (bld_ls2str built);
    Tty.ifpairln "in queue" (bld_q2str queued)

  let all t =
    status Build.(ls_ready t, ls_built t, ls_queue t)

  let manp need units =
    if need > 0 then
    Units.(count Men units)
    |> min need
    |> manp2str
    |> sprintf "%s worked in construction"
    |> Tty.writeln

  let supply need avlb =
    if need > 0 then
    sprintf "construction costs %s, have %s" (sup2str need) (sup2str avlb)
    |> Tty.writeln
end

module Leader = struct
  let died t =
    sprintf "%s has died, was %s" (ldr2full t) (ldr2status t)
    |> Tty.writeln

  let to_full t =
    if Leader.is_alive t then ldr2full t else ""

  let to_fled t units =
    let units = units2str units in
    match to_full t with
    | "" -> sprintf "%s has fled" units
    | ldr -> sprintf "%s fled with %s" ldr units

  let lvup t =
    sprintf "%s is now %s" (ldr2first t) (ldr2status t)
    |> Tty.writeln
end

module Combat = struct
  let stats atk def dmg =
    let attack = sprintf "%s attack" (power2str atk) in
    let defense = sprintf "%s dr" (percent2str def) in
    let damage = sprintf "%s damage" (power2str dmg) in
    sprintf "%s -> %s -> %s" attack defense damage

  let retreat ldr units enemies =
    Tty.writeln (Leader.to_fled ldr units);
    Tty.pairln "remaining enemies" (units2str enemies)

  let win ldr casualty died =
    Tty.pairln "casualty" (units2str casualty);
    if died then Leader.died ldr

  let outcome (module O : Combat.Outcome) units enemies ldr =
    Tty.pairln "attacking" (units2str enemies);
    Tty.pairln "defending" (units2str units);
    Tty.ifpairln "leader" (Leader.to_full ldr);
    if O.cav_too_many
    then Tty.writeln "too many cavalry, defense reduced";
    Tty.writeln (stats O.attack O.defense O.damage);
    if O.retreat then retreat ldr O.units O.enemies
    else win ldr O.units O.ldr_died
end

let can_barrage w =
  let open Direct.CanBarrage in
  function
    | No Leader -> ()
    | No Weather -> Tty.spln (weather2str w) "prevents arrow barrage"
    | Yes -> ()

let support ls =
  let f res =
    if res = Resource.empty then "nothing"
    else res2str res
  in
  ls
  |> List.map (fun (nat, res) -> (nation2str nat, f res))
  |> List.map (fun (nat, res) -> sprintf "%s sent %s" nat res)
  |> Tty.writelns
