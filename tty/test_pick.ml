open Game
open Printf

module S = State.Make(Random)
module A = Attack.Make(S)
module Dist = Units.Dist(S.Dice)
module Fill = Units.Fill(S.Dice)
module FillC = Units.FillCount(S.Dice)

let cap = float_of_string Sys.argv.(1)
let cap' = truncate cap

let units =
  Units.(make 10 Men |> add 10 Cavalry |> add 10 Templar |> add 10 Knight)

let () =
  Random.self_init ();
  Printf.printf "dist: %s\n" (Dist.from cap units |> Convert.units2str);
  Printf.printf "fill: %s\n" (Fill.from cap units |> Convert.units2str);
  Printf.printf "fillc: %s\n" (FillC.from cap' units |> Convert.units2str)
