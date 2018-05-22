open Game_defs

module Resource = Game_resource

type count = int
type party = (enemy * count)
type resource = Resource.t

let scouting_cost =
  Resource.make (Resource.Supply 10)

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

let to_mp (enemy, count) =
  let p = power_of enemy in
  truncate (p *. float count)

let damage enemies =
  enemies
  |> List.map to_mp
  |> List.fold_left (+) 0

let better_round x =
  let a = 0.1 *. float x in
  let f = if Dice.chance 0.5 then floor else ceil in
  truncate (f a)

let vague_round x =
  let a = float x in
  let x1 = 10. ** floor (log10 a) in
  let x5 = 5. *. x1 in
  let b = if a < x5 then x1 else x5 in
  truncate b

let scout enemies =
  let f (e, c) = (e, better_round c) in
  List.map f enemies

let vague_scout enemies =
  let f (e, c) = (e, vague_round c) in
  List.map f enemies

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
