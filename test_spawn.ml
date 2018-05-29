open Game
open Printf

let turn = ref 1

let e2s = Convert.enemies2str

let rec loop i =
  if i > 0
  then begin
    let e = Enemy.spawn !turn in
    printf "turn %d\nvague: %s\nscout: %s\nactual: %s\n"
      !turn
      (Enemy.vague_scout e |> e2s)
      (Enemy.scout e |> e2s)
      (e2s e);
    incr turn;
    loop (pred i)
  end

let () =
  let count = try int_of_string Sys.argv.(1) with _ -> 5 in
  Random.self_init ();
  loop count
