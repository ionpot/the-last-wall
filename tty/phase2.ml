module Phase = Game.Phase2

module Make (S : Game.State.S) = struct
  let input =
    let open Phase.Input in
    let check f x = if x > 0 then f x else x in
    function
      | Ballista (avlb, have) ->
          Ballista (check (Prompt.ballista have) avlb, have)
      | Berserker (_, avlb) ->
          let accept =
            if avlb > 0 then Prompt.berserker avlb else false
          in Berserker (accept, avlb)
      | Build avlb ->
          let module Prompt = Prompt.Build(S) in
          S.Build.return Print.Build.all;
          Build (S.Build.return (Prompt.from avlb))
      | Dervish count -> Dervish (check Prompt.dervish count)
      | LeaderNew ls -> LeaderNew (Prompt.new_leader ls)
      | Knight count -> Knight (check Prompt.knight count)
      | Mercs count -> Mercs (Prompt.mercs count)
      | Nations chosen -> Nations (Prompt.nations chosen)
      | Ranger count -> Ranger (check Prompt.ranger count)
      | Templar count -> Templar (check Prompt.templar count)
      | Trade _ -> Trade (Prompt.trade ())
      | Volunteers count -> Volunteers (check Prompt.volunteers count)

  let output =
    let open Convert in
    let open Phase.Output in
    function
      | Attack (_, rp) -> Tty.pairln "seen" (report2str rp)
      | Blessing res ->
          res2str res
          |> (if S.Deity.is Game.Deity.Lerota
              then (fun x -> x)
              else str2none)
          |> Tty.ifpairln "blessing"
      | BuildManp m -> S.Units.return (Print.Build.manp m)
      | BuildStatus s -> ()
      | BuildSupply s -> S.Supply.return (Print.Build.supply s)
      | Cavalry c -> ()
      | Defeat -> Tty.writeln "defeat"
      | Disease x -> Print.disease x |> S.Leader.return
      | Facilities ls -> Tty.ifpairln "facilities" (facs2str ls)
      | Starvation units -> Tty.ifpairln "starvation" (units2str units)
      | Support s -> Print.support s
      | Turn t -> Tty.lnwriteln (turn2str t)
      | Upkeep sup -> Tty.pairln "upkeep" (sup2str sup)
end

module After (S : Status.S) = struct
  let input =
    let open Phase.Input in
    function
      | Ballista (n, _) -> if n > 0 then S.res ()
      | Berserker (ok, n) -> if n > 0 then begin if ok then S.berserker (); S.res () end
      | Dervish n -> if n > 0 then begin S.dervish (); S.res () end
      | LeaderNew ls -> S.new_leader ls
      | Knight n -> if n > 0 then S.res ()
      | Mercs n -> if n > 0 then S.res ()
      | Ranger n -> if n > 0 then begin S.ranger (); S.res () end
      | Templar n -> if n > 0 then begin S.templar (); S.res () end
      | Volunteers n -> if n > 0 then S.res ()
      | _ -> ()

  let output =
    let open Phase.Output in
    function
      | Blessing res -> if res <> Game.Resource.empty then S.res ()
      | BuildSupply s -> if s > 0 then S.res ()
      | Cavalry n -> if n > 0 then begin S.cavalry n; S.res () end
      | Facilities ls -> if ls <> [] then S.res ()
      | Starvation _
      | Support _
      | Upkeep _ -> S.res ()
      | _ -> ()
end
