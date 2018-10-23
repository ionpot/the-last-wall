module CL = Check_leader

type event =
  | Attack of Enemy.party list
  | Casualty of Defs.manpower
  | Defeat
  | End
  | Leader of CL.event
  | SendScouts of bool
  | Smite of Enemy.party
  | Victory

module type S = Phase.S with type event_def := event

module Make (M : State.S) : S = struct
  module Casualty = Check_casualty.Make(M)
  module Divine = Check_divine.Make(M)
  module Ldr = CL.Make(M)

  let ask_scouting () =
    SendScouts (M.is_scouting ())

  let first () =
    match M.get_enemies () with
    | [] -> ask_scouting ()
    | ls -> Attack ls

  let leader_won = function
    | Some ldr -> Leader.won ldr
    | None -> ()

  let apply = function
    | Attack enemies -> ()
    | Casualty manp -> M.sub_manp manp
    | Defeat
    | End -> ()
    | Leader CL.Died _ -> M.ldr_died ()
    | Leader CL.LvUp ldr -> M.set_ldr ldr
    | SendScouts yes -> M.set_scouting yes
    | Smite party -> M.map_enemies (Enemy.reduce party)
    | Victory -> leader_won (M.get_ldr ())

  let check_casualty enemies =
    match Casualty.check enemies with
    | Some x -> Casualty x
    | None -> ask_scouting ()

  let check_smite enemies =
    match Divine.smite enemies with
    | Some party -> Smite party
    | None -> check_casualty enemies

  let check_ldr () =
    match Ldr.check () with
    | Some x -> Leader x
    | None -> ask_scouting ()

  let next = function
    | Attack enemies -> check_smite enemies
    | Smite _ -> M.with_enemies check_casualty
    | Casualty _ ->
        if Casualty.is_victory () then Victory else Defeat
    | Victory -> check_ldr ()
    | Leader _ -> ask_scouting ()
    | SendScouts _
    | Defeat
    | End -> End
end
