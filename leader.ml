let chance_of mp dmg =
  float dmg /. float (mp * 2)

let damage_of =
  let int_of : Outcome.t -> int = function
    | Manpower i
    | Both (_, i) -> i
    | Supply _
    | Leader _ -> 0
  in
  let f total atk = total - int_of atk.Attack.outcome in
  List.fold_left f 0

let has_died (alist: Attack.t list) (g: Game.t) =
  let dmg = damage_of alist in
  let mp = g.wall.manpower in
  let c = chance_of mp dmg in
  Dice.chance c
