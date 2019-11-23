module Phase = Game.Phase3

module Make (S : Game.State.S) = struct
  let input =
    let open Phase.Input in
    function
      | Barrage (_, status) ->
          (S.Weather.return Print.barrage_status) status;
          Barrage (Prompt.barrage status, status)
      | Scout _ -> Scout (Prompt.scout ())

  let output =
    let open Convert in
    let open Phase.Output in
    function
      | Attack ->
          Print.Combat.begins
          |> S.Bonus.return
          |> S.Units.return
          |> S.Enemy.return
          |> S.Leader.return
      | Ballista x -> Print.ballista x
      | Barraged x -> Tty.pairln "barraged" (units2str x |> str2none)
      | Combat x ->
          Print.Combat.outcome x
          |> S.Leader.return
      | Cyclops x -> Print.cyclops x
      | Defeat -> Tty.writeln "defeat"
      | Fear x -> Print.fear x
      | HitRun x -> Print.hit_run x
      | LevelUp -> S.Leader.return Print.Leader.lvup
      | NoAttack -> ()
      | NoEnemies -> Tty.writeln "no enemies left"
      | Revive (x, _) -> Tty.ifpairln "revived" (units2str x)
      | Smite x -> Tty.pairln "smite" (units2str x |> str2none)
      | Victory -> ()
end

module After (S : Status.S) = struct
  let input =
    let open Phase.Input in
    function
      | Barrage _
      | Scout _ -> ()

  let output =
    let open Phase.Output in
    function
      | Ballista (n, _, _) -> if n > 0 then S.enemies ()
      | Barraged _ -> S.enemies ()
      | Cyclops (n, _, _) -> if n > 0 then S.units ()
      | Fear x -> if Convert.units2bool x then S.units ()
      | Smite x -> if Convert.units2bool x then S.enemies ()
      | Victory -> S.units ()
      | _ -> ()
end
