open Defs

type t = Skeleton | Orc | Demon
type count = int
type party = (t * count)
type resource = Resource.t

let t_list =
  [Skeleton; Orc; Demon]

let abundance_of = function
  | Skeleton -> 1.25
  | Orc -> 0.6
  | Demon -> 0.3

let chance_of = function
  | Skeleton -> 0.8
  | Orc -> 0.6
  | Demon -> 0.4

let power_of = function
  | Skeleton -> 0.3
  | Orc -> 0.5
  | Demon -> 2.0

let count_of party = snd party
let type_of party = fst party

let has_type t p =
  type_of p = t

let same_type a b =
  type_of a = type_of b

let sub a b =
  if same_type a b
  then (type_of a, count_of a - count_of b)
  else a

let to_mp (enemy, count) =
  let p = power_of enemy in
  truncate (p *. float count)

let damage enemies =
  enemies
  |> List.map to_mp
  |> List.fold_left (+) 0

let can_spawn turn enemy =
  let a = 0.1 *. float turn in
  let b = chance_of enemy in
  Dice.chance (a +. b)

let get_count turn enemy =
  let abundance = abundance_of enemy in
  let minimum = 10. *. abundance in
  let amount =
    let x = 1.3 *. float turn in
    abundance *. x *. log x
  in
  let x = ceil (minimum +. amount) |> truncate in
  Dice.deviate x (x / 4)

let spawn turn =
  let a = List.filter (can_spawn turn) t_list in
  let b = List.map (get_count turn) a in
  List.combine a b

let roll_smite p =
  let x = Dice.between 10 30 in
  let y = count_of p in
  type_of p, min x y

let smite enemies =
  match List.filter (has_type Skeleton) enemies with
  | [] -> None
  | party :: _ -> Some (roll_smite party)

let reduce party enemies =
  enemies
  |> List.map (fun p -> sub p party)
  |> List.filter (fun p -> count_of p > 0)
