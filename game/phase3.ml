module CL = Check_leader

type event =
  | Attack of Enemy.party list
  | Barrage of bool
  | Barraged of Enemy.party
  | Casualty of Defs.manpower * Defs.manpower
  | Defeat
  | End
  | Fort of Defs.manpower * Defs.manpower
  | Leader of CL.event
  | SendScouts of bool
  | Smite of Enemy.party
  | Victory

module type S = Phase.S with type event_def := event

module Make (M : State.S) : S = struct
  module Casualty = Check_casualty.Make(M)
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
    | Barrage x -> M.Barraging.set_to x
    | Barraged party -> M.map_enemies (Enemy.reduce party)
    | Casualty (men, cav) ->
        M.sub_manp men;
        M.Cavalry.sub cav
    | Defeat
    | End -> ()
    | Fort (men, cav) ->
        M.bld_raze Building.Fort;
        M.set_manp men;
        M.Cavalry.set cav
    | Leader CL.Died _ -> M.ldr_died ()
    | Leader CL.LvUp ldr -> M.set_ldr ldr
    | SendScouts yes -> M.set_scouting yes
    | Smite party -> M.map_enemies (Enemy.reduce party)
    | Victory -> leader_won (M.get_ldr ())

  let check_casualty () =
    let module C = Check_casualty in
    match M.with_enemies Casualty.check with
    | C.Loss (men, cav) -> Casualty (men, cav)
    | C.Fort (men, cav) -> Fort (men, cav)
    | C.Ok -> ask_scouting ()

  let check_barrage () =
    match M.get_ldr () with
    | Some _ -> Barrage (M.Barraging.get ())
    | None -> check_casualty ()

  let check_barraged () =
    let module B = Barrage.Check(M) in
    match B.value with
    | Some party -> Barraged party
    | None -> check_casualty ()

  let check_smite () =
    let module S = Smite.Check(M) in
    match S.value with
    | Some party -> Smite party
    | None -> check_barrage ()

  let check_ldr () =
    match Ldr.check () with
    | Some x -> Leader x
    | None -> ask_scouting ()

  let next = function
    | Attack _ -> check_smite ()
    | Smite _ -> check_barrage ()
    | Barrage _ -> check_barraged ()
    | Barraged _ -> check_casualty ()
    | Casualty _ ->
        if Casualty.is_victory () then Victory else Defeat
    | Fort _ -> Victory
    | Victory -> check_ldr ()
    | Leader _ -> ask_scouting ()
    | SendScouts _
    | Defeat
    | End -> End
end
