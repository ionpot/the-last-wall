module Phase = Game.Phase2

module Make (S : Game.State.S) = struct
  let input =
    let open Phase.Input in
    let check f x = if x > 0 then f x else x in
    function
      | Ballista avlb -> Ballista (check Prompt.ballista avlb)
      | Barracks _ -> Barracks (Prompt.barracks ())
      | BarrageTrain (ok, cost) ->
          BarrageTrain (Prompt.barrage_train ok cost, cost)
      | Berserker avlb -> Berserker (check Prompt.berserker avlb)
      | Build avlb ->
          let nat = S.Nation.get () in
          S.Build.return (Print.Build.all nat);
          Build (Prompt.Build.from nat avlb)
      | Dervish count -> Dervish (check Prompt.dervish count)
      | LeaderNew x -> LeaderNew (Prompt.Leader.pair x)
      | Knight count -> Knight (check Prompt.knight count)
      | Mercs count -> Mercs (Prompt.mercs count)
      | Nations chosen ->
          let module Prompt = Prompt.Nations(S) in
          Nations (Prompt.from chosen)
      | Ranger count -> Ranger (check Prompt.ranger count)
      | Sodistan sup -> Sodistan (check Prompt.sodistan sup)
      | Templar count -> Templar (check Prompt.templar count)
      | Trade _ -> Trade (Prompt.trade ())
      | Veteran count -> Veteran (check Prompt.veteran count)
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
      | BuildManp m ->
          Print.Build.manp m
          |> S.Bonus.return
          |> S.Units.return
      | BuildStatus s -> ()
      | BuildSupply s -> Print.Build.supply s
      | Cavalry c -> ()
      | Defeat -> Tty.writeln "defeat"
      | Disease x -> Print.disease x |> S.Leader.return
      | Facilities x ->
          let nat = S.Nation.get () in
          Print.facilities nat x
      | FearEnd x -> Print.fear_end x
      | Mishap x -> Print.mishap x
      | Starvation x -> Print.starvation x
      | Support s -> Print.support s
      | Turn t -> Tty.lnwriteln (turn2str t)
      | Upkeep sup -> Tty.pairln "upkeep" (sup2str sup)
end

module After (S : Status.S) = struct
  let input =
    let open Phase.Input in
    function
      | Ballista n -> if n > 0 then S.res ()
      | BarrageTrain (ok, _) -> if ok then S.res ()
      | Berserker n -> if n > 0 then begin S.berserker (); S.res () end
      | Dervish n -> if n > 0 then begin S.dervish (); S.res () end
      | LeaderNew x -> S.new_leader x
      | Knight n -> if n > 0 then S.res ()
      | Mercs n -> if n > 0 then S.res ()
      | Ranger n -> if n > 0 then begin S.ranger (); S.res () end
      | Sodistan n -> if n > 0 then S.res ()
      | Templar n -> if n > 0 then begin S.templar (); S.res () end
      | Veteran n
      | Volunteers n -> if n > 0 then S.res ()
      | _ -> ()

  let output =
    let open Phase.Output in
    function
      | Blessing res -> if res <> Game.Resource.empty then S.res ()
      | BuildSupply s -> if s > 0 then S.res ()
      | Cavalry n -> if n > 0 then begin S.cavalry n; S.res () end
      | Facilities x -> S.facilities x
      | FearEnd x -> if Convert.units2bool x then S.res ()
      | Starvation x -> if Convert.starve2bool x then S.res ()
      | Support _
      | Upkeep _ -> S.res ()
      | _ -> ()
end
