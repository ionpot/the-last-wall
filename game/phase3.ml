module CL = Check_leader

type event =
  | Attack of Enemy.party list
  | Casualty of Resource.t
  | Defeat
  | End
  | Leader of CL.event
  | SendScouts of bool
  | Smite of Enemy.party
  | Victory

module type S = Phase.S with type event_def := event

module Make (M : State.S) : S = struct
  module Divine = Check_divine.Make (M)
  module Ldr = CL.Make (M)

  let ask_scouting () =
    SendScouts (M.is_scouting ())

  let first () =
    let enemies = M.get_enemies () in
    if enemies = []
    then ask_scouting ()
    else Attack enemies

  let leader_won = function
    | Some ldr -> Leader.won ldr
    | None -> ()

  let apply = function
    | Attack enemies -> ()
    | Casualty res -> M.sub_res res
    | Defeat
    | End -> ()
    | Leader CL.Died _ -> M.ldr_died ()
    | Leader CL.LvUp ldr -> M.set_ldr ldr
    | SendScouts yes -> M.set_scouting yes
    | Smite party ->
        M.get_enemies ()
        |> Enemy.reduce party
        |> M.set_enemies
    | Victory -> leader_won (M.get_ldr ())

  let mitigate loss ldr =
    let x = Leader.mitigate loss ldr in
    Resource.(loss -- x)

  let casualty_from enemies =
    let loss = Enemy.damage enemies in
    match M.get_ldr () with
    | Some ldr -> mitigate loss ldr
    | None -> loss

  let to_casualty enemies =
    Casualty (casualty_from enemies)

  let check_smite enemies =
    match Divine.smite enemies with
    | Some party -> Smite party
    | None -> to_casualty enemies

  let is_victory casualty =
    let res = M.get_res () in
    Resource.(has_manp (res -- casualty))

  let check_ldr () =
    match Ldr.check () with
    | Some x -> Leader x
    | None -> ask_scouting ()

  let next = function
    | Attack enemies -> check_smite enemies
    | Smite _ -> to_casualty @@ M.get_enemies ()
    | Casualty res ->
        if is_victory res then Victory else Defeat
    | Victory -> check_ldr ()
    | Leader _ -> ask_scouting ()
    | SendScouts _
    | Defeat
    | End -> End
end
