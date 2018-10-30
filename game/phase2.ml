type event =
  | Blessing of Resource.t
  | Build of Building.t list
  | Built of Building.t list
  | Defeat
  | End
  | LeaderNew of Leader.t
  | Mercs of Defs.manpower * bool
  | Nations of Nation.t list
  | Needs of Buildings.queued list
  | Scout of Check_scouting.report
  | Starvation of Resource.t
  | Support of Nation.support list
  | Turn of Defs.turn
  | Upkeep of Resource.t

module type S = Phase.S with type event_def := event

module Make (M : State.S) : S = struct
  module Divine = Check_divine.Make(M)
  module Scouting = Check_scouting.Make(M)
  module Support = Check_support.Make(M)
  module Upkeep = Check_upkeep.Make(M)

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
    | Scout _ -> ()
    | Starvation res ->
        M.sub_res res;
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
    | Upkeep res -> M.sub_res res

  let to_scouting () =
    Scout (Scouting.get_report ())

  let to_support () =
    Support (Support.get ())

  let to_upkeep () =
    Upkeep (Upkeep.get ())

  let check_blessing () =
    match Divine.blessing () with
    | Some res -> Blessing res
    | None -> to_support ()

  let check_defeat () =
    if M.has_manp ()
    then to_scouting ()
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
    match Upkeep.get_starvation () with
    | Some res -> Starvation res
    | None -> to_scouting ()

  let next = function
    | Turn _ -> check_built ()
    | Built _ -> check_needs ()
    | Needs _ -> check_leader ()
    | LeaderNew _ -> to_upkeep ()
    | Upkeep _ -> check_starvation ()
    | Starvation _ -> check_defeat ()
    | Scout _ -> Nations (M.get_nats ())
    | Nations _ -> check_blessing ()
    | Blessing _ -> to_support ()
    | Support _ -> Build []
    | Build _ -> check_mercs ()
    | Mercs _
    | Defeat
    | End -> End
end
