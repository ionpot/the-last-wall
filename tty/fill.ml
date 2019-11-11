open Game
open Printf

module S = State.Make(Random)
module A = Attack.Make(S)
module Fill = Dist.Fill(S.Dice)

let cap = float_of_string Sys.argv.(1)
let cap' = truncate cap

let base = S.Bonus.return Power.base

let units =
  Units.(make 60 Men |> add 20 Cavalry |> add 10 Templar |> add 10 Dervish)

let () =
  Random.self_init ();
  let picked, rest = Fill.from cap base units in
  Printf.printf "picked: %s\n" (Convert.units2str picked);
  Printf.printf "rest: %s\n" (Convert.units2str rest)
