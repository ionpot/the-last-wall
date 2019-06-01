module Phase = Game.Phase3

module Make (S : Game.State.S) = struct
  let input =
    let open Phase.Input in
    function
      | Barrage _ -> Barrage (Prompt.barrage ())
      | Scout _ -> Scout (Prompt.scout ())

  let output =
    let open Convert in
    let open Phase.Output in
    function
      | Attack ->
          Print.Combat.begins
          |> S.Units.return
          |> S.Enemy.return
          |> S.Leader.return
      | Barraged x -> Tty.pairln "barraged" (barrage2str x)
      | CanBarrage x -> (S.Weather.return Print.can_barrage) x
      | Combat x ->
          Print.Combat.outcome x
          |> S.Leader.return
      | Defeat -> Tty.writeln "defeat"
      | LevelUp -> S.Leader.return Print.Leader.lvup
      | NoAttack -> ()
      | NoEnemies -> Tty.writeln "no enemies left"
      | Revive x -> Tty.pairln "revived" (units2str x)
      | Smite x -> Tty.pairln "smite" (smite2str x)
      | Victory -> ()
end

module After (S : Status.S) = struct
  let input =
    let open Phase.Input in
    function
      | Barrage yes -> if yes then S.enemies ()
      | Scout _ -> ()

  let output =
    let open Phase.Output in
    function
      | Smite _ -> S.enemies ()
      | Victory -> S.units ()
      | _ -> ()
end
