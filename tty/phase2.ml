module Phase = Game.Phase2

module Make (S : Game.State.S) = struct
  let input =
    let open Phase.Input in
    function
      | Build avlb ->
          S.Build.return Print.Build.all;
          Build (S.Build.return (Prompt.build avlb))
      | Dervish count -> Dervish (Prompt.dervish count)
      | Mercs count -> Mercs (Prompt.mercs count)
      | Nations chosen -> Nations (Prompt.nations chosen)
      | Trade _ -> Trade (Prompt.trade ())

  let output =
    let open Convert in
    let open Phase.Output in
    function
      | Attack (_, rp) -> Tty.pairln "seen" (report2str rp)
      | Blessing res -> Tty.pairln "blessing" (res2str res)
      | BuildManp m -> Print.Build.manp m
      | BuildStatus s -> Print.Build.status s
      | BuildSupply s -> Print.Build.supply s
      | Cavalry c -> Tty.pairln "cavalry" (string_of_int c)
      | Defeat -> Tty.writeln "defeat"
      | LeaderNew ldr -> Tty.pairln "new leader" (ldr2full ldr)
      | Market sup -> Tty.pairln "market" (sup2str sup)
      | Starvation units -> Tty.pairln "starvation" (units2str units)
      | Support s -> Print.support s
      | Turn t -> Tty.lnwriteln (turn2str t)
      | Upkeep sup -> Tty.pairln "upkeep" (sup2str sup)
end
