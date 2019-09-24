open Game
open Printf

module S = State.Make(Random)
module A = Attack.Make(S)
module Dist = Units.Dist
module DistRoll = Dist.Roll(S.Dice)

let cap = float_of_string Sys.argv.(1)
let cap' = truncate cap

let units =
  Units.(make 60 Men |> add 20 Cavalry |> add 10 Templar |> add 10 Dervish)

let () =
  Random.self_init ();
  let r = DistRoll.from cap units in
  Printf.printf "casualty: %s\n" (Dist.outcome r |> Convert.units2str);
  Printf.printf "absorbed: %s\n" (Dist.absorbed r |> Convert.power2str);
  Printf.printf "healed: %s\n" (Dist.healed r |> Convert.power2str)
