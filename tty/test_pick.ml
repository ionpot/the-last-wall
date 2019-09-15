open Game
open Printf

module S = State.Make(Random)
module A = Attack.Make(S)
module Dist = Units.Dist
module DistRoll = Dist.Roll(S.Dice)
module Fill = Units.Fill(S.Dice)
module FillC = Units.FillCount(S.Dice)

let cap = float_of_string Sys.argv.(1)
let cap' = truncate cap

let units =
  Units.(make 10 Men |> add 10 Cavalry |> add 10 Templar |> add 10 Knight)

let () =
  Random.self_init ();
  let r = DistRoll.from cap units in
  Printf.printf "dist: %s\n" (Dist.outcome r |> Convert.units2str);
  Printf.printf "absorbed: %s\n" (Dist.absorbed r |> Convert.power2str);
  Printf.printf "healed: %s\n" (Dist.healed r |> Convert.power2str);
  Printf.printf "fill: %s\n" (Fill.from cap units |> Convert.units2str);
  Printf.printf "fillc: %s\n" (FillC.from cap' units |> Convert.units2str)
