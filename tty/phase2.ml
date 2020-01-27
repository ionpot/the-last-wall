module Phase = Game.Phase2
module Research = Game.Research
module Units = Game.Units

module Make (S : Game.State.S) = struct
  let input =
    let open Phase.Input in
    let check f x = if x > 0 then f x else x in
    let promote k = check (Prompt.promote k) in
    function
      | Ballista avlb -> Ballista (check (Prompt.siege Units.Ballista) avlb)
      | Barracks _ -> Barracks (Prompt.barracks ())
      | BarrageTrain (ok, cost) ->
          BarrageTrain (Prompt.barrage_train ok cost, cost)
      | Berserker avlb -> Berserker (promote Units.Berserker avlb)
      | Build avlb ->
          let nat = S.Nation.get () in
          S.Build.return (Print.Build.all nat);
          Build (Prompt.Build.from nat avlb)
      | Dervish avlb -> Dervish (promote Units.Dervish avlb)
      | Harcher avlb -> Harcher (promote Units.Harcher avlb)
      | LeaderNew x -> LeaderNew (Prompt.Leader.pair x)
      | Knight avlb -> Knight (promote Units.Knight avlb)
      | Mercs count -> Mercs (Prompt.mercs count)
      | Nations chosen ->
          let module Prompt = Prompt.Nations(S) in
          Nations (Prompt.from chosen)
      | Novice avlb -> Novice (promote Units.Novice avlb)
      | Ranger avlb -> Ranger (promote Units.Ranger avlb)
      | Research (_, avlb) -> Research (Prompt.research avlb, avlb)
      | Sodistan sup -> Sodistan (check Prompt.sodistan sup)
      | Templar avlb -> Templar (promote Units.Templar avlb)
      | Temple count -> Temple (check Prompt.temple count)
      | Trade _ -> Trade (Prompt.trade ())
      | Veteran avlb -> Veteran (promote Units.Veteran avlb)
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
      | BuildStatus _ -> ()
      | BuildSupply s -> Print.Build.supply s
      | Cavalry _ -> ()
      | Defeat -> Tty.writeln "defeat"
      | Disease x -> Print.disease x |> S.Leader.return
      | Facilities x ->
          let nat = S.Nation.get () in
          Print.facilities nat x
      | FearEnd x -> Print.fear_end x
      | Mishap x -> Print.mishap x
      | ResearchProgress x -> Print.research_progress x
      | ResearchStatus x -> Print.research_status x
      | Starvation x -> Print.starvation x
      | Support s -> Print.support s
      | Turn t -> Tty.lnwriteln (turn2str t)
      | Upkeep sup -> Tty.pairln "upkeep" (sup2str sup)
end

module After (S : Status.S) = struct
  let promote k n =
    if n > 0 then begin S.promote k; S.res () end

  let input =
    let open Phase.Input in
    function
      | Ballista n -> if n > 0 then S.res ()
      | BarrageTrain (ok, _) -> if ok then S.res ()
      | Berserker n -> promote Units.Berserker n
      | Dervish n -> promote Units.Dervish n
      | Harcher n -> promote Units.Harcher n
      | LeaderNew x -> S.new_leader x
      | Knight n -> promote Units.Knight n
      | Mercs n -> if n > 0 then S.res ()
      | Novice n -> promote Units.Novice n
      | Ranger n -> promote Units.Ranger n
      | Sodistan n -> if n > 0 then S.res ()
      | Templar n -> promote Units.Templar n
      | Temple n -> if n > 0 then S.res ()
      | Veteran n -> promote Units.Veteran n
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
      | ResearchProgress (module P : Research.Progress) ->
          if not (Research.Set.is_empty P.started) then S.res ()
      | Starvation x -> if Convert.starve2bool x then S.res ()
      | Support _
      | Upkeep _ -> S.res ()
      | _ -> ()
end
