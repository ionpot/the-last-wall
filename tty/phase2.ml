module Phase = Game.Phase2

module Make (S : Game.State.S) = struct
  let input =
    let open Phase.Input in
    let check f x = if x > 0 then f x else x in
    function
      | Build avlb ->
          let module Prompt = Prompt.Build(S) in
          S.Build.return Print.Build.all;
          Build (S.Build.return (Prompt.from avlb))
      | Dervish count -> Dervish (check Prompt.dervish count)
      | Mercs count -> Mercs (Prompt.mercs count)
      | Nations chosen -> Nations (Prompt.nations chosen)
      | Ranger count -> Ranger (check Prompt.ranger count)
      | Templar count -> Templar (check Prompt.templar count)
      | Trade _ -> Trade (Prompt.trade ())

  let output =
    let open Convert in
    let open Phase.Output in
    function
      | Attack (_, rp) -> Tty.pairln "seen" (report2str rp)
      | Blessing res -> Tty.pairln "blessing" (res2str res |> str2none)
      | BuildManp m -> S.Units.return (Print.Build.manp m)
      | BuildStatus s -> ()
      | BuildSupply s -> S.Supply.return (Print.Build.supply s)
      | Cavalry c -> ()
      | Defeat -> Tty.writeln "defeat"
      | LeaderNew ldr -> Tty.pairln "new leader" (ldr2full ldr)
      | Market sup -> Tty.pairln "market" (sup2str sup)
      | Starvation units -> Tty.pairln "starvation" (units2str units)
      | Support s -> Print.support s
      | Turn t -> Tty.lnwriteln (turn2str t)
      | Upkeep sup -> Tty.pairln "upkeep" (sup2str sup)
end

module After (S : Status.S) = struct
  let input =
    let open Phase.Input in
    function
      | Dervish n -> if n > 0 then begin S.dervish (); S.res () end
      | Mercs n -> if n > 0 then S.res ()
      | Ranger n -> if n > 0 then begin S.ranger (); S.res () end
      | Templar n -> if n > 0 then begin S.templar (); S.res () end
      | _ -> ()

  let output =
    let open Phase.Output in
    function
      | Blessing res -> if res <> Game.Resource.empty then S.res ()
      | BuildSupply s -> if s > 0 then S.res ()
      | Cavalry c -> S.cavalry c; S.res ()
      | Market _
      | Starvation _
      | Support _
      | Upkeep _ -> S.res ()
      | _ -> ()
end
