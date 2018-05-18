open Game_defs

type count = int
type party = (enemy * count)

let abundance_of = function
  | Skeleton -> 1.0
  | Orc -> 0.8
  | Demon -> 0.6

let chance_of = function
  | Skeleton -> 0.8
  | Orc -> 0.6
  | Demon -> 0.4

let power_of = function
  | Skeleton -> 0.2
  | Orc -> 0.5
  | Demon -> 0.8

let count_of party = snd party
let type_of party = fst party

let can_spawn turn enemy =
  let a = 0.1 *. float turn in
  let b = chance_of enemy in
  Dice.chance (a +. b)

let get_count turn enemy =
  let base = max 0 (10 - turn) in
  let abundance = abundance_of enemy in
  let amount =
    abundance *. exp (float base)
    |> ceil |> truncate
  in
  let x = base + amount in
  Dice.deviate x (x / 2)

let spawn turn =
  let a = List.filter (can_spawn turn) enemy_list in
  let b = List.map (get_count turn) a in
  List.combine a b

let to_mp (enemy, count) =
  let p = power_of enemy in
  truncate (p *. float count)

let damage army =
  army
  |> List.map to_mp
  |> List.fold_left (+) 0
