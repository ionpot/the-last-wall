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

  let manp m =
    if m > 0 then
    Tty.spln "construction used" (manp2str m)

  let supply s =
    if s > 0 then
    Tty.spln "construction costs" (sup2str s)
end

module Leader = struct
  let died t =
    sprintf "%s has died, was %s" (ldr2full t) (ldr2status t)
    |> Tty.writeln

  let lvup t =
    sprintf "%s is now %s" (ldr2first t) (ldr2status t)
    |> Tty.writeln
end

module Combat = struct
  let stats atk def dmg =
    let attack = sprintf "%s attack power" (power2str atk) in
    let defense = sprintf "%s damage reduction" (percent2str def) in
    let damage = sprintf "%s damage" (power2str dmg) in
    sprintf "%s - %s = %s" attack defense damage

  let retreat ldr units enemies =
    sprintf "%s fled with %s" (ldr2first ldr) (units2str units)
    |> Tty.writeln;
    Tty.pairln "remaining enemies" (units2str enemies)

  let win ldr casualty died =
    Tty.pairln "casualty" (units2str casualty);
    if died then Leader.died ldr

  let outcome (module O : Combat.Outcome) units enemies ldr =
    Tty.pairln "attacking" (units2str enemies);
    Tty.pairln "defending" (units2str units);
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
