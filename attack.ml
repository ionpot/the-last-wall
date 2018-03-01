type t =
  { enemy : Enemy.t;
    count : int;
    outcome : Outcome.t;
  }

let fleft x = x *. 0.1
let sof = Enemy.stats_of

let chance_of e turn =
  let { chance } = sof e in
  let f = fleft (float turn) in
  chance +. f

let enemy_of g =
  let e = Enemy.random () in
  let c = chance_of e g.turn in
  if Dice.chance c
  then Some e
  else None

let count_of e turn =
  let a = (float turn) in
  let b = 1.0 +. Random.float (10.0 *. a) in
  let { modifier = m } = sof e in
  let c = fleft a in
  (b *. (m +. c)) |> ceil |> truncate

let outcome_of enemy count w =
  let { power } = sof enemy in
  let i = if w.leader then 0 else 1 in
  let damage = (count + i) * power in
  let loss = Dice.deviate damage count in
  Manpower ~-loss

let make enemy g =
  let count = count_of enemy g.turn in
  { enemy;
    count;
    outcome = outcome_of enemy count g.wall;
  }

let rec build_list g i ls =
  if i > 0 then
    let new_ls = match enemy_of g with
      | Some e -> (make e g) :: ls
      | None -> ls
    in
    build_list g (pred i) new_ls
  else ls

(* interface *)
let build game =
  let ls = build_list game game.turn [] in
  if Leader.has_died ls game
  then (Leader false) :: ls
  else ls
