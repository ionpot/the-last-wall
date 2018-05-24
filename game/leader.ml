type ltype = Aristocrat | Expert | Warrior
type level = int

type t = (ltype * level)

let ltypes = [Aristocrat; Expert; Warrior]

let make () =
  let ty = Listx.pick_from ltypes in
  let lv = Dice.between 3 6 in
  (ty, lv)

let lives () =
  Dice.chance 0.95

let type_of t = fst t
let level_of t = snd t
