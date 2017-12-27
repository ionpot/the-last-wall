type t =
  { enemy : Enemy.t;
    count : int;
  }

let fleft x = x *. 0.1
let sof = Enemy.stats_of

let chance_of e w =
  let c = (sof e).chance in
  let f = fleft (float w.age) in
  c +. f

let pick_enemy w =
  let e = Enemy.random () in
  let c = chance_of e w in
  if Dice.chance c
  then Some e
  else None

let pick_count e w =
  let age = (float w.age) in
  let a = 1.0 +. Random.float (10.0 *. age) in
  let b = (sof e).modifier in
  let c = fleft age in
  (a *. (b +. c)) |> ceil |> truncate

let to_t e w =
  { enemy = e;
    count = pick_count e w;
  }

let rec build_list w i ls =
  if i > 0 then
    let new_ls = match pick_enemy w with
      | Some e -> (to_t e w) :: ls
      | None -> ls
    in
    build_list w (pred i) new_ls
  else ls

(* interface *)
let build wall =
  build_list wall wall.age []

let outcome_of a =
  let power = (sof a.enemy).power in
  let damage = a.count * power in
  let loss = Dice.deviate damage a.count in
  Outcome.make 0 -~loss
