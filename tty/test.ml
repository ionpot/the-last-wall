open Game
open Printf

module S = State.Make(Random)
module A = Attack.Make(S)

let rec loop i =
  S.Turn.add 1;
  let turn = S.Turn.get () in
  if i > 0
  then begin
    let e = A.roll turn in
    printf "turn %d -> %s\n" turn (Convert.units2mnpstr e);
    loop (pred i)
  end

let () =
  let count = try int_of_string Sys.argv.(1) with _ -> 5 in
  Random.self_init ();
  S.Harpy.set 0.;
  loop count
