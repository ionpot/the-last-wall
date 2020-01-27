module Phase = Game.Phase3
module Units = Game.Units

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
      | Ballista x -> Print.siege Units.Ballista x
      | Barraged x -> Tty.pairln "barraged" (units2str x |> str2none)
      | Combat x ->
          Print.Combat.outcome x
          |> S.Leader.return
      | Cyclops x -> Print.siege Units.Cyclops x
      | Defeat -> Tty.writeln "defeat"
      | Fear x -> Print.fear x
      | HitRun x -> Print.hit_run x
      | LevelUp -> S.Leader.return Print.Leader.lvup
      | Mangonel x -> Print.siege Units.Mangonel x
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
      | Ballista (x, _) -> if Convert.units2bool x then S.enemies ()
      | Barraged _ -> S.enemies ()
      | Cyclops (x, _) -> if Convert.units2bool x then S.units ()
      | Fear x -> if Convert.units2bool x then S.units ()
      | Mangonel (x, _) -> if Convert.units2bool x then S.enemies ()
      | Smite x -> if Convert.units2bool x then S.enemies ()
      | Victory -> S.units ()
      | _ -> ()
end
