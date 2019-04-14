type t =
  { skel : Defs.count;
    orc : Defs.count;
    demon : Defs.count
  }
type kind = Skeleton | Orc | Demon
type expr = (Defs.count * kind)
type report =
  | Accurate of expr list
  | Vague of (Defs.count * kind list)

let empty =
  { skel = 0;
    orc = 0;
    demon = 0
  }

let kinds =
  [Skeleton; Orc; Demon]

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

let count_of t = function
  | Skeleton -> t.skel
  | Orc -> t.orc
  | Demon -> t.demon

let set_count t x = function
  | Skeleton -> { t with skel = x }
  | Orc -> { t with orc = x }
  | Demon -> { t with demon = x }

let to_count kind t =
  count_of t kind

let has_kind t kind =
  count_of t kind > 0

let combine a b =
  { skel = a.skel + b.skel;
    orc = a.orc + b.orc;
    demon = a.demon + b.demon
  }

let sub a b =
  { skel = Number.sub a.skel b.skel;
    orc = Number.sub a.orc b.orc;
    demon = Number.sub a.demon b.demon
  }

let to_power t kind =
  power_of kind *. float (count_of t kind)

let damage t =
  List.map (to_power t) kinds
  |> List.fold_left (+.) 0.

let t_from_ls = function
  | [skel; orc; demon] -> { skel; orc; demon }
  | ls -> Pick.invalid ls

let pick power t =
  let a = List.map (count_of t) kinds in
  let b = List.map power_of kinds in
  List.combine a b
  |> Pick.random power
  |> t_from_ls

let discard power t =
  if power > damage t then empty
  else if power > 0. then sub t (pick power t)
  else t

let find count kind t =
  let x = count_of t kind in
  min x count

let reduce count kind t =
  let x = count_of t kind in
  let y = Number.sub x count in
  set_count t y kind

let report_of t =
  [t.skel, Skeleton; t.orc, Orc; t.demon, Demon]
  |> List.map (fun (x, k) -> try_round x, k)

let which t =
  List.filter (has_kind t) kinds

let sum_report_of t =
  let total = t.skel + t.orc + t.demon in
  let seen = which t in
  try_round total, seen

let to_report scouting t =
  if scouting
  then Accurate (report_of t)
  else Vague (sum_report_of t)

let can_spawn turn kind =
  let a = 0.1 *. float (Number.sub turn 1) in
  let b = chance_of kind in
  Dice.chance (a +. b)

let get_count turn kind =
  let abundance = abundance_of kind in
  let minimum = 10. *. abundance in
  let amount =
    let x = 1.3 *. float (turn + 3) in
    abundance *. x *. log x
  in
  let x = ceil (minimum +. amount) |> truncate in
  Dice.deviate x (x / 4)

let spawn turn =
  let set t kind = set_count t (get_count turn kind) kind in
  List.filter (can_spawn turn) kinds
  |> List.fold_left set empty
