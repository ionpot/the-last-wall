open Defs

type event =
  | Blessing of Resource.t
  | Build of Building.t list
  | Built of Building.t list
  | Defeat
  | End
  | LeaderNew of Leader.t
  | Mercs of manpower * bool
  | Nations of Nation.t list
  | Needs of Buildings.queued list
  | Report of Enemy.report
  | ReportSum of Enemy.sum_report
  | Starvation of manpower
  | Support of Nation.support list
  | Turn of turn
  | Upkeep of supply

module type S = Phase.S with type event_def := event

module Make (M : State.S) : S = struct
  module Support = Check_support.Make(M)

  let first () =
    Turn (M.get_turn () + 1)

  let buy_mercs mercs =
    M.buy_manp_with (Merc.buy mercs)

  let apply = function
    | Blessing res -> M.add_res res
    | Build x -> M.build x; M.bld_supp ()
    | Built _
    | Defeat
    | End -> ()
    | LeaderNew ldr -> M.set_ldr ldr
    | Mercs (mercs, accept) ->
        if accept then buy_mercs mercs
    | Nations nats -> M.set_nats nats
    | Needs _
    | Report _
    | ReportSum _ -> ()
    | Starvation mp ->
        M.sub_manp mp;
        M.clr_supp ()
    | Support supp_list ->
        Nation.total_of supp_list
        |> M.add_res
    | Turn x ->
        M.set_turn x;
        M.bld_manp ();
        M.bld_tick ();
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
    let s = M.is_scouting () in
    let sp = M.get_supp () in
    Upkeep (Upkeep.cost_from s sp)

  let check_blessing () =
    match M.with_deity Deity.blessing_of with
    | Some res -> Blessing res
    | None -> to_support ()

  let check_defeat () =
    if M.has_manp ()
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

  let check_mercs () =
    match Merc.roll () with
    | Some mercs -> Mercs (mercs, false)
    | None -> End

  let check_starvation () =
    match M.with_supp Upkeep.check_starvation with
    | Some manp -> Starvation manp
    | None -> to_report ()

  let next = function
    | Turn _ -> check_built ()
    | Built _ -> check_needs ()
    | Needs _ -> check_leader ()
    | LeaderNew _ -> to_upkeep ()
    | Upkeep _ -> check_starvation ()
    | Starvation _ -> check_defeat ()
    | Report _
    | ReportSum _ -> Nations (M.get_nats ())
    | Nations _ -> check_blessing ()
    | Blessing _ -> to_support ()
    | Support _ -> Build []
    | Build _ -> check_mercs ()
    | Mercs _
    | Defeat
    | End -> End
end
