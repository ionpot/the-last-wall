open Game
open Printf

module S = State.Make(Random)
module Damage = Dist.Damage(S.Dice)
module D1 = Damage(struct
  let full_absorb = false
  let use_ratio = true
end)
module D2 = Damage(struct
  let full_absorb = true
  let use_ratio = false
end)

let cap = float_of_string Sys.argv.(1)
let cap' = truncate cap

let base = S.Bonus.return Power.base

let enemy =
  Units.(make 60 Skeleton |> add 40 Orc |> add 20 Demon |> add 10 Harpy |> add 5 Cyclops |> add 1 Dullahan)

let units =
  Units.(make 60 Men |> add 20 Cavalry |> add 10 Templar |> add 10 Dervish)

let dist f a b =
  let r = f cap base a b in
  Printf.printf "casualty: %s\n" (Dist.outcome r |> Convert.units2str);
  Printf.printf "absorbed: %s\n" (Dist.absorbed r |> Convert.power2str);
  Printf.printf "healed: %s\n" (Dist.healed r |> Convert.power2str)

let () =
  Random.self_init ();
  dist D1.from enemy units;
  print_newline ();
  dist D2.from units enemy
