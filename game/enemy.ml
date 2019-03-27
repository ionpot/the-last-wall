type t = Skeleton | Orc | Demon
type count = int
type party = (t * count)
type report =
  | Accurate of party list
  | Vague of (count * t list)

let t_list =
  [Skeleton; Orc; Demon]

let make ls =
  List.map (fun (n, t) -> t, n) ls

let try_round x =
  if x > 10 then Dice.round x else x

let abundance_of = function
  | Skeleton -> 1.25
  | Orc -> 0.6
  | Demon -> 0.3

let chance_of = function
  | Skeleton -> 0.8
  | Orc -> 0.6
  | Demon -> 0.4

let power_of = function
  | Skeleton -> 0.5
  | Orc -> 1.0
  | Demon -> 2.0

let count_of party = snd party
let type_of party = fst party

let has_type t p =
  type_of p = t

let same_type a b =
  has_type (type_of a) b

let with_count c party =
  (type_of party, c)

let map_count f party =
  with_count (count_of party |> f) party

let has_count party =
  count_of party > 0

let report_of parties =
  List.map (map_count try_round) parties

let sub a b =
  if same_type a b
  then with_count (count_of a - count_of b) a
  else a

let sum_report_of parties =
  let f a party = a + count_of party in
  let total = List.fold_left f 0 parties in
  let seen = List.map type_of parties in
  try_round total, seen

let to_report parties scouting =
  if scouting
  then Accurate (report_of parties)
  else Vague (sum_report_of parties)

let to_mp (enemy, count) =
  power_of enemy *. float count

let damage parties =
  parties
  |> List.map to_mp
  |> List.fold_left (+.) 0.

let can_spawn turn enemy =
  let a = 0.1 *. float (Number.sub turn 1) in
  let b = chance_of enemy in
  Dice.chance (a +. b)

let get_count turn enemy =
  let abundance = abundance_of enemy in
  let minimum = 10. *. abundance in
  let amount =
    let x = 1.3 *. float (turn + 3) in
    abundance *. x *. log x
  in
  let x = ceil (minimum +. amount) |> truncate in
  Dice.deviate x (x / 4)

let spawn turn =
  let a = List.filter (can_spawn turn) t_list in
  let b = List.map (get_count turn) a in
  List.combine a b

let find count t parties =
  match List.filter (has_type t) parties with
  | [] -> None
  | party :: _ -> Some (map_count (min count) party)

let reduce party parties =
  parties
  |> List.map (fun p -> sub p party)
  |> List.filter has_count
