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
      | Attack -> Tty.pairln "attack" (S.Enemy.return units2str)
      | Barraged x -> Tty.pairln "barraged" (barrage2str x)
      | CanBarrage x -> (S.Weather.return Print.can_barrage) x
      | Combat x ->
          Print.Combat.outcome x
          |> S.Units.return
          |> S.Enemy.return
          |> S.Leader.return
      | Defeat -> Tty.writeln "defeat"
      | LevelUp -> S.Leader.return Print.Leader.lvup
      | NoAttack -> ()
      | NoEnemies -> Tty.writeln "no enemies left"
      | Revive x -> Tty.pairln "revived" (units2str x)
      | Smite x -> Tty.pairln "smite" (smite2str x)
      | Victory -> ()
end

(*
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
  *)
