type event =
  | Attack of Enemy.party list
  | Casualty of Resource.t
  | Defeat
  | End
  | LeaderDied of Leader.t
  | LeaderLvup of Leader.t
  | SendScouts of bool
  | Smite of Enemy.party
  | Victory

module type S = Phase.S with type event_def := event

module Make(M : State.S) : S = struct

  let ask_scouting () =
    SendScouts (M.is_scouting ())

  let first () =
    let enemies = M.get_enemies () in
    if enemies = []
    then ask_scouting ()
    else Attack enemies

  let apply = function
    | Attack enemies -> ()
    | Casualty res -> M.sub_res res
    | Defeat
    | End -> ()
    | LeaderDied _ -> M.ldr_died ()
    | LeaderLvup ldr
    | LeaderNew ldr -> M.set_ldr ldr
    | SendScouts yes -> M.set_scouting yes
    | Smite party ->
        M.get_enemies ()
        |> Enemy.reduce party
        |> M.set_enemies
    | Victory -> M.ldr_won ()

  let mitigate loss =
    let x = M.get_ldr () |> Leader.mitigate loss in
    Resource.(loss -- x)

  let casualty_from enemies =
    let loss = Enemy.damage enemies in
    if M.ldr_alive () then mitigate loss else loss

  let to_casualty enemies =
    Casualty (casualty_from enemies)

  let check_smite enemies =
    match Enemy.smite enemies with
    | Some party -> Smite party
    | None -> to_casualty enemies

  let check_lvup ldr =
    if Leader.can_lvup ldr
    then LeaderLvup (Leader.lvup ldr)
    else ask_scouting ()

  let check_ldr () =
    let ldr = M.get_ldr () in
    if Leader.lives ()
    then check_lvup ldr
    else LeaderDied ldr

  let is_victory casualty =
    let res = M.get_res () in
    Resource.(has_manp (res -- casualty))

  let next = function
    | Attack enemies ->
        if M.get_deity () = Deity.Lerota
        then check_smite enemies
        else to_casualty enemies
    | Smite _ -> to_casualty @@ M.get_enemies ()
    | Casualty res ->
        if is_victory res then Victory else Defeat
    | Victory ->
        if M.ldr_alive ()
        then check_ldr ()
        else ask_scouting ()
    | LeaderLvup _
    | LeaderDied _ -> ask_scouting ()
    | SendScouts _
    | Defeat
    | End -> End
end
