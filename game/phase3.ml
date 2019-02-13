module Output = struct
  type event =
    | BuildSupply of Cond.BuildSupply.t
    | Starting of Direct.Starting.t
    | Support of Direct.Support.t

  type input =
    | Build of Input.Build.t
    | Nations of Input.Nations.t
    | Scout of Input.Scout.t

  type notify = unit

  type t = (event, input, notify) Phase.output
end

module type Steps = Phase.Steps with
  type Output.event = Output.event and
  type Output.input = Output.input and
  type Output.notify = Output.notify and
  type Output.t = Output.t

module Steps : Steps = struct
  module Output = Output

  type cond = BuildSupply
  type direct = Starting | Support
  type input = Build | Nations | Scout
  type notify = unit

  type event = (cond, direct, input, notify) Phase.etype

  type t = event Phase.step list

  let steps =
    let open Phase in
    [ Do (Direct Starting);
      Do (Input Nations);
      Do (Direct Support);
      Do (Input Build);
      Do (Cond BuildSupply);
      Do (Input Scout)
    ]

  let is_end _ = false

  module Apply (S : State.S) = struct
    let event = function
      | Output.BuildSupply x -> S.bld_supp x
      | Output.Starting x -> S.add_res x
      | Output.Support x -> S.add_res (Nation.total_of x)

    let input = function
      | Output.Build x -> S.build x
      | Output.Nations x -> S.set_nats x
      | Output.Scout x -> S.set_scouting x
  end

  module Check = struct
    let cond : cond -> (module Event.Check) = function
      | BuildSupply -> (module Cond.BuildSupply.Check)

    let input : input -> (module Event.Check) = function
      | Build -> (module Input.Build.Check)
      | Nations -> (module Input.Nations.Check)
      | Scout -> (module Input.Scout.Check)

    let notify : notify -> (module Event.Check) = fun () ->
      (module Event.Never)
  end

  module Make (S : State.S) = struct
    let cond = let open Cond in function
      | BuildSupply ->
          let module M = BuildSupply.Make(S) in Output.BuildSupply M.value

    let direct = let open Direct in function
      | Starting -> let module M = Starting.Make(S) in Output.Starting M.value
      | Support -> let module M = Support.Make(S) in Output.Support M.value

    let input = let open Input in function
      | Build -> let module M = Build.Make(S) in Output.Build M.value
      | Nations -> let module M = Nations.Make(S) in Output.Nations M.value
      | Scout -> let module M = Scout.Make(S) in Output.Scout M.value

    let notify () = ()
  end
end

(*module CL = Check_leader

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
end*)
