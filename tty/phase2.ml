let input x = x
let output _ = ()

(*
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
  *)
