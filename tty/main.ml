open Convert
open Printf

module CL = Game.Check_leader
module S = Game.State.Make(struct
  Random.self_init ()
  let deity = Prompt.deity ()
  let leader = Prompt.leader ()
end)
module M = Game.Main.Make(S)

let status =
  let get () = S.get_manp (), S.get_supp (), S.Cavalry.get () in
  let x = ref (get ()) in
  fun () ->
    let y = get () in
    if !x <> y then begin
      x := y;
      Tty.pairln "status" (status2str y)
    end

let leader = function
  | CL.Died ldr -> Print.leader "leader died, was" ldr
  | CL.LvUp ldr -> Print.leader "leader is now" ldr

let phase3 evt update =
  let open Game.Phase3 in
  match evt with
  | Attack ls ->
      Tty.pairln "attack" (enemies2str ls);
      Print.defense (S.get_manp ()) (S.Cavalry.get ()) (S.get_ldr ())
  | Barrage _ ->
      let b = Prompt.barrage () in
      update (Barrage b)
  | Barraged p -> Tty.pairln "barraged" (party2str p)
  | Casualty (men, cav) -> Tty.pairln "casualty" (units2str men cav)
  | Defeat
  | End -> ()
  | Fort (men, cav) -> Tty.pairln "fort" (units2str men cav)
  | Leader x -> leader x
  | SendScouts _ ->
      let s = Prompt.scouting () in
      Print.scouting_chosen s;
      update (SendScouts s)
  | Smite p -> Tty.pairln "smitten" (party2str p)
  | Victory -> ()

let phase2 evt update =
  let open Game.Phase2 in
  match evt with
  | Blessing res -> Tty.pairln "blessing" (res2str res)
  | Build _ ->
      let bs = S.with_bld Prompt.build in
      Print.bld_chosen bs;
      update (Build bs)
  | BuildManpower m -> Tty.pairln "construction" (manp2str m)
  | BuildSupply s -> Tty.pairln "construction" (sup2str s)
  | BuildTick -> ()
  | Built bs -> Tty.pairln "built" (buildings2str bs)
  | Cavalry x -> Tty.pairln "cavalry" (string_of_int x)
  | Defeat
  | End -> ()
  | LeaderNew ldr -> Tty.pairln "new leader" (leader2str ldr)
  | Market sup -> Tty.pairln "market" (sup2str sup)
  | Mercs (x, _) ->
      let ok = Prompt.mercs x in
      update (Mercs (x, ok))
  | Nations ls ->
      let ns = Prompt.nations ls in
      Print.nations_chosen ns;
      update (Nations ns)
  | Needs q -> Print.queued q
  | Report ls -> Tty.pairln "seen" (report2str ls)
  | ReportSum x -> Tty.pairln "seen" (sum2str x)
  | Starvation (men, cav) -> Tty.pairln "starvation" (units2str men cav)
  | Support ls -> Print.support ls
  | Turn x -> Tty.lnwriteln ("turn " ^ string_of_int x)
  | Upkeep sup -> Tty.pairln "upkeep" (sup2str sup)

let phase1 evt update =
  let open Game.Phase1 in
  match evt with
  | Build _ ->
      let bs = S.with_bld Prompt.build in
      Print.bld_chosen bs;
      update (Build bs)
  | BuildSupply x -> Print.bld_supp x
  | End -> ()
  | Nations x ->
      let ns = Prompt.nations x in
      Print.nations_chosen ns;
      update (Nations ns)
  | SendScouts _ ->
      let s = Prompt.scouting () in
      Print.scouting_chosen s;
      update (SendScouts s)
  | Starting res ->
      Tty.pairln "starting" (res2str res)
  | Support ls ->
      Print.support ls

let rec loop evt =
  let next x =
    let y = M.next x in
    status ();
    Tty.flush ();
    loop y
  in
  let open Game.Main in
  match evt with
  | End -> Tty.fin ()
  | Ph1 x ->
      let e = ref x in
      phase1 x (fun y -> e := y);
      next (Ph1 !e)
  | Ph2 x ->
      let e = ref x in
      phase2 x (fun y -> e := y);
      next (Ph2 !e)
  | Ph3 x ->
      let e = ref x in
      phase3 x (fun y -> e := y);
      next (Ph3 !e)

let () =
  Print.deity_chosen (S.get_deity ());
  Print.leader_chosen (S.get_ldr ());
  M.first () |> loop
