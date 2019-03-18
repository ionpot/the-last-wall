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

  let is_end () = false

  module Apply (State : State.S) = struct
    let event = function
      | BuildSupply x -> State.bld_supp x
      | Starting x -> State.add_res x
      | Support x -> State.add_res (Nation.total_of x)

    let input = function
      | Build x -> State.build x
      | Nations x -> State.set_nats x
      | Scout x -> State.set_scouting x
  end
end

module Steps = Steps.Phase1

module type S = Phase.S with
  type Output.event = Output.event and
  type Output.input = Output.input and
  type Output.notify = Output.notify and
  type Steps.cond = Steps.cond and
  type Steps.direct = Steps.direct and
  type Steps.input = Steps.input and
  type Steps.notify = Steps.notify

module S : S = struct
  module Output = Output
  module Steps = Steps

  open Steps

  module Check = struct
    let cond : cond -> (module Event.Check) = function
      | BuildSupply -> (module Cond.BuildSupply.Check)

    let input : input -> (module Event.Check) = function
      | Build -> (module Input.Build.Check)
      | Nations -> (module Input.Nations.Check)
      | Scout -> (module Input.Scout.Check)

    let notify : notify -> (module Event.Check) =
      fun () -> (module Event.Never)
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
(*open Defs

type event =
  | Blessing of Resource.t
  | Build of Building.t list
  | BuildManpower of manpower
  | BuildSupply of supply
  | BuildTick
  | Built of Building.t list
  | Cavalry of manpower
  | Defeat
  | End
  | LeaderNew of Leader.t
  | Market of supply
  | Mercs of manpower * bool
  | Nations of Nation.t list
  | Needs of Buildings.queued list
  | Report of Enemy.report
  | ReportSum of Enemy.sum_report
  | Starvation of manpower * manpower
  | Support of Nation.support list
  | Turn of turn
  | Upkeep of supply

module type S = Phase.S with type event_def := event

module Make (M : State.S) : S = struct
  module Support = Check_support.Make(M)

  let first () =
    Turn (M.get_turn () + 1)

  let apply = function
    | Blessing res -> M.add_res res
    | Build x -> M.build x
    | BuildManpower x -> M.bld_manp x
    | BuildSupply x -> M.bld_supp x
    | BuildTick -> M.bld_tick ()
    | Built _ -> ()
    | Cavalry x ->
        M.Cavalry.add x;
        M.sub_manp x;
        M.sub_supp x
    | Defeat
    | End -> ()
    | LeaderNew ldr -> M.set_ldr ldr
    | Market x -> M.add_supp x
    | Mercs (mercs, accept) -> if accept then M.supp2manp mercs
    | Nations nats -> M.set_nats nats
    | Needs _
    | Report _
    | ReportSum _ -> ()
    | Starvation (men, cavs) ->
        M.sub_manp men;
        M.Cavalry.sub cavs;
        M.clr_supp ()
    | Support supp_list ->
        Nation.total_of supp_list
        |> M.add_res
    | Turn x ->
        M.set_turn x;
        M.ldr_tick ();
        M.set_enemies (Enemy.spawn x)
    | Upkeep sp -> M.sub_supp sp

  let to_report () =
    let e = M.get_enemies () in
    if M.is_scouting ()
    then Report (Enemy.report_of e)
    else ReportSum (Enemy.sum_report_of e)

  let to_support () =
    Support (Support.get ())

  let to_upkeep () =
    let module U = Upkeep.Check(M) in
    Upkeep U.total

  let check_market () =
    let module S = Market.Check(M) in
    match S.value with
    | Some res -> Market res
    | None -> Nations (M.get_nats ())

  let check_blessing () =
    let module B = Blessing.Check(M) in
    match B.value with
    | Some res -> Blessing res
    | None -> check_market ()

  let check_defeat () =
    if M.has_manp () || M.Cavalry.ptv ()
    then to_report ()
    else Defeat

  let check_leader () =
    if M.need_ldr ()
    then LeaderNew (Leader.random ())
    else to_upkeep ()

  let check_needs () =
    match M.bld_queued () with
    | [] -> check_leader ()
    | ls -> Needs ls

  let check_built () =
    match M.built () with
    | [] -> check_needs ()
    | ls -> Built ls

  let check_bld_manp () =
    let cost = M.bld_manp_cost () in
    if cost > 0
    then BuildManpower cost
    else BuildTick

  let check_mercs () =
    match Merc.roll () with
    | Some mercs -> Mercs (mercs, false)
    | None -> End

  let check_cavalry () =
    let module S = Cavalry.Check(M) in
    match S.value with
    | Some count -> Cavalry count
    | None -> check_mercs ()

  let check_bld_supp () =
    let cost = M.bld_supp_cost () in
    if cost > 0
    then BuildSupply cost
    else check_cavalry ()

  let check_starvation () =
    let module U = Upkeep.Starvation(M) in
    match U.value with
    | Some (men, cav) -> Starvation (men, cav)
    | None -> to_report ()

  let next = function
    | Turn _ -> check_bld_manp ()
    | BuildManpower _ -> BuildTick
    | BuildTick -> check_built ()
    | Built _ -> check_needs ()
    | Needs _ -> check_leader ()
    | LeaderNew _ -> to_upkeep ()
    | Upkeep _ -> check_starvation ()
    | Starvation _ -> check_defeat ()
    | Report _
    | ReportSum _ -> check_blessing ()
    | Blessing _ -> check_market ()
    | Market _ -> Nations (M.get_nats ())
    | Nations _ -> to_support ()
    | Support _ -> Build []
    | Build _ -> check_bld_supp ()
    | BuildSupply _ -> check_cavalry ()
    | Cavalry _ -> check_mercs ()
    | Mercs _
    | Defeat
    | End -> End
end*)
