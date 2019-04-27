let input x = x
let output _ = ()

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
